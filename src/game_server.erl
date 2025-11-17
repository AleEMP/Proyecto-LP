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
    
    % --- ¡ESTA ES LA LÍNEA CRÍTICA QUE FALTABA! ---
    % Establecemos {packet, 4} EN EL SOCKET ACEPTADO.
    % Ahora este socket sabe cómo manejar los prefijos de 4 bytes
    % tanto para enviar como para recibir.
    inet:setopts(Socket, [{packet, 4}]),
    
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
    % 1. NO activamos el socket todavía.
    %    El socket está en modo pasivo {active, false} (heredado de 'listen')
    %    por lo que podemos hacer llamadas de forma segura.

    % 2. Registrar al jugador (self()) en la lógica del juego
    case game_manager:add_player(self()) of
        ok ->
            io:format("PID ~p añadido al juego.~n", [self()]),
            
            % 3. Enviar bienvenida (esto funciona en modo pasivo)
            WelcomeMap = #{
                action => <<"welcome">>,
                my_pid => list_to_binary(pid_to_list(self()))
            },
            send_json(Socket, WelcomeMap), % send_json usa gen_tcp:send, que es síncrono

            % 4. ¡AHORA SÍ! Estamos listos.
            %    Configuramos el socket a modo {active, true}
            %    para que el client_loop pueda recibir mensajes.
            %inet:setopts(Socket, [{active, true}]),
            
            % --- LOG 0 ---
            io:format("~p: [BRIDGE] 'welcome' enviado. Entrando en client_loop...~n", [self()]),
            
            % 5. Iniciar el bucle principal del cliente
            client_loop(Socket);
            
        {error, max_players_reached} ->
            io:format("Rechazado: Máximo de jugadores alcanzado.~n"),
            ErrorMap = #{action => <<"error">>, message => <<"server_full">>},
            send_json(Socket, ErrorMap),
            gen_tcp:close(Socket);

        {error, game_already_in_progress} ->
            io:format("Rechazado: Juego en progreso.~n"),
            ErrorMap = #{action => <<"error">>, message => <<"game_in_progress">>},
            send_json(Socket, ErrorMap),
            gen_tcp:close(Socket)
    end.

%% Bucle principal del manejador de cliente
client_loop(Socket) ->
    
    % 1. Esperar mensajes INTERNOS de Erlang (de la lógica del juego)
    %    con un 'timeout' de 0 para que no bloquee.
    receive
        {send_json_payload, JsonMap} ->
            io:format("~p: [BRIDGE] Recibido payload de la lógica. Enviando a cliente...~n", [self()]),
            send_json(Socket, JsonMap);

        % --- LOG 6 (Catch-all) ---
        UnexpectedMessage ->
            io:format("~p: [BRIDGE] Recibido mensaje INESPERADO: ~p~n", [self(), UnexpectedMessage])
    after 0 ->
        % No había mensajes de Erlang, continuar...
        ok
    end,

    % 2. Ahora, pedir activamente datos de RED (TCP)
    %    Esto le pide al socket que nos dé datos (o espere 0ms si no hay)
    %    Como el socket tiene {packet, 4}, gen_tcp:recv/2 leerá el
    %    prefijo y nos devolverá SÓLO el payload.
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Data} ->
            % ¡Recibimos datos de red!
            handle_json_from_client(Data, Socket),
            client_loop(Socket);
        
        {error, timeout} ->
            % No llegaron datos de red. No pasa nada.
            client_loop(Socket);

        {error, closed} ->
            % El cliente se desconectó
            io:format("~p: [BRIDGE] Cliente desconectado (recv closed).~n", [self()]);
            
        {error, Reason} ->
            % Otro error
            io:format("~p: [BRIDGE] Error de socket (recv): ~p.~n", [self(), Reason])
    end.

%% ============================================================
%% TRADUCCIÓN JSON -> GEN_SERVER:CALL
%% ============================================================

handle_json_from_client(Data, Socket) ->
    MyPID = self(), % El PID de este proceso es el PlayerPID
    io:format("~p: [BRIDGE] Recibido JSON crudo: ~p~n", [MyPID, Data]),
    try jsx:decode(Data, [return_maps]) of
        #{<<"action">> := <<"start_game">>} ->
            io:format("~p: [BRIDGE] JSON Decodificado: START_GAME. Llamando a la lógica...~n", [MyPID]),
            % Llamada síncrona al gen_server
            Result = game_manager:start_game(),
            % El game_manager debería entonces enviar {your_turn} al primer jugador
            case Result of
                ok -> 
                    % El juego comenzó bien.
                    send_json(Socket, #{action => <<"start_ok">>});
                {error, Reason} ->
                    % El juego NO pudo empezar (ej. not_enough_players)
                    send_json(Socket, #{action => <<"start_error">>, reason => Reason})
            end;
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

% Envía un mapa de Erlang como JSON.
send_json(Socket, ErlangMap) ->
    try 
        % 1. Codificar a JSON (binario)
        Payload = jsx:encode(ErlangMap),
        
        % 2. Enviar SÓLO el Payload.
        % La opción {packet, 4} que pusimos en 'accept_loop'
        % se encarga de añadir el prefijo automáticamente.
        gen_tcp:send(Socket, Payload)
        
    catch
        _:Error ->
            io:format("Error al enviar JSON: ~p~n", [Error])
    end.