-module(game_server).
-export([start/0]). % Única función pública

-export([accept_loop/1, client_handler/1]). % Loops internos
-export([send_json/2]). % API para enviar JSON

-include_lib("kernel/include/inet.hrl"). % Para #inet_tcp
-include("virus_defs.hrl"). % Para definiciones

-define(PORT, 4000).

%% ============================================================
%% INICIO DEL SERVIDOR
%% ============================================================
start() ->
    % 1. Iniciar la Lógica del Juego (El gen_server)
    % Asumimos que game_server1 se llama game_manager
    case game_manager:start_link() of
        {ok, _Pid} -> io:format("Lógica del juego (game_manager) iniciada.~n");
        {error, {already_started, _Pid}} -> io:format("Lógica del juego (game_manager) ya estaba iniciada.~n");
        Error -> io:format("ERROR al iniciar game_manager: ~p~n", [Error])
    end,
    
    % 2. Iniciar el Socket de Escucha TCP
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor TCP (Puente) iniciado en el puerto ~p~n", [?PORT]),
    
    % 3. Iniciar el bucle de aceptación
    accept_loop(ListenSocket).

%% ============================================================
%% ACEPTAR CLIENTES
%% ============================================================
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Conexión aceptada...~n"),
    
    % Por cada cliente, creamos un proceso "manejador" que vivirá
    % mientras el cliente esté conectado.
    spawn(fun() -> client_handler(Socket) end),
    
    accept_loop(ListenSocket). % Recursión de accept_loop/1

%% ============================================================
%% MANEJADOR DE CLIENTE (EL PUENTE REAL)
%% ============================================================

% Este es el proceso que representa a un jugador dentro de Erlang.
% Su 'self()' es el PID que usará el game_manager.
client_handler(Socket) ->
    % 1. Configurar el socket para modo {active, true}
    % Esto nos enviará los datos como mensajes {tcp, Socket, Data}
    % Es crucial para poder recibir mensajes de Erlang y del socket
    % en el mismo bucle de 'receive'.
    inet:setopts(Socket, [{active, true}]),

    % 2. Registrar al jugador (self()) en la lógica del juego
    case game_manager:add_player(self()) of
        ok ->
            io:format("PID ~p añadido al juego.~n", [self()]),
            
            % 3. Enviar bienvenida al cliente
            % ¡Importante! Le decimos al cliente cuál es su PID (como string)
            % para que sepa quién es en los mensajes de estado.
            WelcomeMsg = jsx:encode(#{
                action => <<"welcome">>,
                my_pid => list_to_binary(pid_to_list(self()))
            }),
            send_json(Socket, WelcomeMsg),

            % 4. Iniciar el bucle principal del cliente
            client_loop(Socket);
            
        {error, max_players_reached} ->
            io:format("Rechazado: Máximo de jugadores alcanzado.~n"),
            ErrorMsg = jsx:encode(#{action => <<"error">>, message => <<"server_full">>}),
            send_json(Socket, ErrorMsg),
            gen_tcp:close(Socket);

        {error, game_already_in_progress} ->
            io:format("Rechazado: Juego en progreso.~n"),
            ErrorMsg = jsx:encode(#{action => <<"error">>, message => <<"game_in_progress">>}),
            send_json(Socket, ErrorMsg),
            gen_tcp:close(Socket)
    end.

%% Bucle principal del manejador de cliente
client_loop(Socket) ->
    receive
       % --------------------------------------------------
        % A: Mensajes del Socket TCP (Vienen del Cliente Python)
        % --------------------------------------------------
        {tcp, Socket, Data} ->
            handle_json_from_client(Data, Socket),
            client_loop(Socket); % Continuar el bucle

        % --------------------------------------------------
        % B: Mensajes del Servidor de Lógica (game_manager)
        % --------------------------------------------------

        % ESTE ES EL ÚNICO MENSAJE QUE LA LÓGICA ENVÍA AHORA
        {send_json_payload, JsonMap} ->
            send_json(Socket, JsonMap),
            client_loop(Socket);

        % --------------------------------------------------
        % C: Mensajes de Desconexión
        % --------------------------------------------------
        {tcp_closed, Socket} ->
            io:format("~p: Cliente desconectado.~n", [self()]);
            % TODO: (A futuro) Aquí deberías notificar a game_manager
            % que este PID se fue (ej. game_manager:remove_player(self()))

        {tcp_error, Socket, Reason} ->
            io:format("~p: Error de socket ~p.~n", [self(), Reason])
    end.

%% ============================================================
%% TRADUCCIÓN JSON -> GEN_SERVER:CALL
%% ============================================================

handle_json_from_client(Data, Socket) ->
    MyPID = self(), % El PID de este proceso es el PlayerPID
    
    try jsx:decode(Data, [return_maps]) of
        #{<<"action">> := <<"start_game">>} ->
            io:format("~p: Cliente pide iniciar juego.~n", [MyPID]),
            % Llamada síncrona al gen_server
            game_manager:start_game(); 
            % El game_manager debería entonces enviar {your_turn} al primer jugador

        #{<<"action">> := <<"play_card">>,
          <<"target_pid">> := TargetPIDBin, % El cliente nos dice el PID del objetivo
          <<"card">> := CardMap,             % La carta (como mapa)
          <<"player_color">> := PlayerColorBin, % Color origen (transplant)
          <<"target_color">> := TargetColorBin  % Color destino (órgano, virus, etc)
         } ->
            
            % --- Traducción de datos de JSON a Erlang ---
            
            % Convertir el PID objetivo de binario/string a PID real
            % ¡ESTO ES PELIGROSO! (ver nota abajo).
            % Es mejor si el cliente envía un índice (0, 1, 2)
            % y aquí lo buscamos en el estado del game_manager.
            % PERO para mantener tu lógica intacta, usamos el PID.
            TargetPID = list_to_pid(binary_to_list(TargetPIDBin)),
            
            % Convertir el Mapa de carta a Record #card
            Card = #card{
                type = maps:get(<<"type">>, CardMap),
                color = maps:get(<<"color">>, CardMap),
                name = maps:get(<<"name">>, CardMap)
            },
            
            PlayerColor = binary_to_atom(PlayerColorBin, utf8),
            TargetColor = binary_to_atom(TargetColorBin, utf8),

            io:format("~p: Cliente pide jugar ~p en ~p (target: ~p)~n", 
                [MyPID, Card#card.name, TargetColor, TargetPID]),

            % Llamada síncrona a la lógica
            Result = game_manager:play_card(MyPID, TargetPID, Card, PlayerColor, TargetColor),
            
            % (Opcional) Enviar una respuesta de 'ok' o 'error' al cliente
            case Result of
                {ok, _} -> send_json(Socket, #{action => <<"play_ok">>});
                {error, Reason} -> send_json(Socket, #{action => <<"play_error">>, reason => Reason})
            end;

        Other ->
            io:format("~p: JSON no reconocido: ~p~n", [MyPID, Other])

    catch
        _:Error ->
            io:format("~p: Error de JSON: ~p. Datos: ~p~n", [MyPID, Error, Data])
    end.

%% ============================================================
%% FUNCIÓN AUXILIAR DE ENVÍO
%% ============================================================

% Envía un mapa de Erlang como JSON, usando el formato {packet, 4}
send_json(Socket, ErlangMap) ->
    try 
        % 1. Codificar a JSON (binario)
        Payload = jsx:encode(ErlangMap),
        
        % 2. Crear prefijo de 4 bytes (Big-endian)
        LengthPrefix = binary:encode_unsigned(byte_size(Payload), big),
        
        % 3. Enviar Prefijo + Payload
        gen_tcp:send(Socket, [LengthPrefix, Payload])
        
    catch
        _:Error ->
            io:format("Error al enviar JSON: ~p~n", [Error])
    end.