%%% ============================================================
%%% Servidor de Juego
%%% ============================================================
-module(game_server).
-export([start/0, accept_loop/1, client_loop/2, unregister_player/1]).
-define(PORT, 4000).
-define(DEFAULT_TURN, "Jugador A").

%%% ============================================================
%%% INICIO DEL SERVIDOR
%%% ============================================================
start() ->
    % [FIX CRÍTICO 1] Usar {packet, 4} y {active, false}
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor Hero Realms iniciado en el puerto ~p~n", [?PORT]),
    accept_loop(ListenSocket).
	
%%% ============================================================
%%% ACEPTAR CLIENTES
%%% ============================================================
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % Creamos el proceso que VIVIRÁ y manejará el socket
    spawn(fun() ->
        % 1. Obtener el nombre
        Player = assign_player_name(self()), % Obtenemos el nombre basado en slots
        
        case Player of
            "Unknown" -> 
                io:format("Conexión rechazada: Servidor lleno.~n"),
                gen_tcp:close(Socket); % Asegura que la conexión rechazada se cierra
            _ ->
                % 2. Registrar el proceso del socket
                register_player(Player), 
                
                % 3. Mensaje de bienvenida (¡Debe contener el rol!)
                WelcomeMsg = io_lib:format("{\"action\":\"welcome\",\"role\":\"~s\",\"message\":\"Bienvenido al servidor Hero Realms!\"}\n", [Player]),
                gen_tcp:send(Socket, WelcomeMsg),

                io:format("~s conectado. Rol asignado: ~s~n", [Player, Player]),

                % 4. Verificar si la partida comienza (ambos conectados)
                case {whereis(player_a), whereis(player_b)} of
                    {PidA, PidB} when is_pid(PidA) andalso is_pid(PidB) ->
                        io:format("INFO: Ambos jugadores conectados. ¡Iniciando partida!~n"),
                        
                        % [FIX] Uso el whereis aquí también para enviar solo si está activo
                        case whereis(game_state) of
                            GamePid when is_pid(GamePid) ->
                                GamePid ! {broadcast_update};
                            _ ->
                                % Si llega aquí, significa que falló inmediatamente al spawn.
                                io:format("ERROR: game_state no está activo para iniciar la partida.~n")
                        end;
                    _ -> 
                        ok
                end,

                client_loop(Socket, Player)
        end % Fin del case
    end), % Fin del spawn/fun
    accept_loop(ListenSocket). % Recursión de accept_loop/1

%%% ============================================================
%%% LOOP DEL CLIENTE
%%% ============================================================
client_loop(Socket, Player) ->
    % Usamos {active, false} o {packet, line} en listen, 
    % por lo que tenemos que llamar a gen_tcp:recv/2
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % 1. Manejar el mensaje (debe decodificar y procesar PING)
            handle_message(Data, Player, Socket),
            % 2. Recursión
            client_loop(Socket, Player);
        
        {error, timeout} ->
            io:format("INFO: ~s aún esperando datos (Timeout).~n", [Player]),
            client_loop(Socket, Player);
            
        {error, closed} ->
            io:format("~s se desconectó.~n", [Player]),
            
            % [FIX CRÍTICO] Notificar a game_state SOLO si existe (evita badarg)
            case whereis(game_state) of
                Pid when is_pid(Pid) ->
                    Pid ! {player_disconnected, Player};
                _ ->
                    ok 
            end,
            
            % [FINAL] Desregistrar y terminar el proceso
            unregister_player(Player),
            ok; % <-- El resultado final de esta cláusula es 'ok'

        {error, Reason} ->
            io:format("ERROR en la conexión de ~s: ~p~n", [Player, Reason]),
            gen_tcp:close(Socket),
            
            % Notificar a game_state y desregistrar
            case whereis(game_state) of
                Pid when is_pid(Pid) ->
                    Pid ! {player_disconnected, Player};
                _ ->
                    ok 
            end,
            
            unregister_player(Player),
            ok % <-- El resultado final de esta cláusula es 'ok'
    end.
	
%%% ============================================================
%%% PROCESAR MENSAJES DEL CLIENTE (CORREGIDO PARA DETECTAR JSON)
%%% ============================================================

handle_message(Data, Player, _Socket) ->
    % 1. Intenta decodificar el binario Data como JSON (jsx:decode)
    case catch jsx:decode(Data, [return_maps]) of
        {'EXIT', _} ->
            io:format("ERROR: Mensaje JSON inválido de ~s: ~p~n", [Player, Data]),
            ok;

        Map when is_map(Map) ->
            % 2. Decodificación exitosa, procesa la acción
            case Map of
                
                % Manejo de la acción PING
                #{<<"action">> := <<"ping">>} -> 
                    io:format("PING OK de ~s~n", [Player]),
                    ok;
                    
                % Manejo de pasar turno (ejemplo)
                #{<<"action">> := <<"end_turn">>} ->
                    io:format("~s pidió pasar el turno.~n", [Player]),
                    % Aquí iría la lógica de cambio de turno
                    ok;

                % Cláusula genérica para acciones no reconocidas
                #{<<"action">> := Action} -> 
                    io:format("ADVERTENCIA: Acción ~p no procesada de ~s~n", [Action, Player]),
                    ok;
                    
                % Cláusula para JSON sin campo 'action'
                _ -> 
                    io:format("ADVERTENCIA: Mensaje JSON sin 'action' de ~s: ~p~n", [Player, Map]),
                    ok
            end;
            
        _ ->
            io:format("ADVERTENCIA: Mensaje no reconocido de ~s: ~p~n", [Player, Data]),
            ok
    end.
%%% ============================================================
%%% OBTENER / CAMBIAR TURNO GLOBAL
%%% ============================================================
get_turn() ->
    case whereis(game_state) of
        undefined ->
            "Jugador A";
        Pid ->
            Pid ! {get_turn, self()},
            receive
                {turn, Turn} -> Turn
            after 1000 ->
                "Jugador A"
            end
    end.

set_turn(NewTurn) ->
    case whereis(game_state) of
        undefined -> ok;
        Pid -> Pid ! {set_turn, NewTurn}, ok
    end.

%%% ============================================================
%%% ENVÍO DE MENSAJES A CLIENTES
%%% ============================================================
send_to_both(Msg) ->
    send_to(player_a, Msg),
    send_to(player_b, Msg).
	
send_to(Name, Msg) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            % Enviar mensaje al proceso del jugador para que él lo envíe por el socket.
            Pid ! {send_message, Msg}; 
        _ ->
            ok
    end.

%%% ============================================================
%%% REGISTRO DE JUGADORES (PETICIÓN DESDE CLIENTE)
%%% ============================================================
assign_player() ->
    game_state ! {assign_request, self()},
    receive
        {assigned, Name} ->
            Name
    after 2000 ->
        io:format("No se pudo asignar jugador a tiempo.~n"),
        "Unknown"
    end.

%%% ============================================================
%%% BUCLE PRINCIPAL DEL ESTADO GLOBAL (TURNOS Y REGISTROS)
%%% ============================================================
turn_loop(CurrentTurn) ->
    receive
        {broadcast_update} -> 
            % Lógica para enviar el turno
            Msg = io_lib:format("{\"action\":\"update\",\"turn\":\"~s\"}\n", [CurrentTurn]),
            send_to_both(Msg),
            turn_loop(CurrentTurn);
            
        {set_turn, NewTurn} ->
            io:format("Turno cambiado a: ~s~n", [NewTurn]),
            turn_loop(NewTurn); 

        {player_disconnected, Name} ->
            io:format("Liberando slot para ~s.~n", [Name]),
            % Si el slot se libera aquí, debes asegurarte que se borra el registro.
            unregister_slot(Name), 
            turn_loop(CurrentTurn);
            
        {get_turn, Caller} ->
            Caller ! {turn, CurrentTurn},
            turn_loop(CurrentTurn);
            
        % <--- FIX CRÍTICO: Esta cláusula evita que el proceso muera por señales de sistema
        UnexpectedMsg ->
            io:format("ADVERTENCIA: turn_loop recibió mensaje no esperado: ~p~n", [UnexpectedMsg]),
            turn_loop(CurrentTurn)

    end.

assign_player_name(_Pid) -> % El PID ya no es necesario aquí
    case {whereis(player_a), whereis(player_b)} of
        {undefined, _} ->
            "Jugador A";
        {_, undefined} ->
            "Jugador B";
        _ ->
            io:format("Ambos jugadores ya conectados. Rechazando nuevo intento.~n"),
            "Unknown"
    end.

%%% ============================================================
%%% FUNCIONES AUXILIARES
%%% ============================================================
unregister_if_exists(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            unregister(Name),
            exit(Pid, kill)
    end.

register_player(Name) ->
    case Name of
        "Jugador A" -> register(player_a, self());
        "Jugador B" -> register(player_b, self());
        _ -> ok
    end.

unregister_slot(Name) ->
    case Name of
        "Jugador A" -> unregister_if_exists(player_a);
        "Jugador B" -> unregister_if_exists(player_b);
        _ -> ok
	end.
	
unregister_player(Name) ->
    io:format("Desregistrando proceso de ~s.~n", [Name]),
    
    % 1. Borra el registro de proceso de Erlang (dondeis(Player) == undefined)
    unregister(list_to_atom(Name)), 
    
    % 2. Llama a la lógica del turn_loop para que libere el slot ('player_a' o 'player_b')
    % Esto lo hace el game_state, no el proceso del cliente
    ok.