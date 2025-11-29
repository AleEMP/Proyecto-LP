-module(game_server).
-export([start/0]). 

-export([accept_loop/1, client_handler/1]). 
-export([send_json/2]).

-include_lib("kernel/include/inet.hrl"). 
-include("virus_defs.hrl"). 

-define(PORT, 4000).

start() ->
    case game_manager:start_link() of
        {ok, _Pid} -> io:format("Lógica del juego (game_manager) iniciada.~n");
        {error, {already_started, _Pid}} -> io:format("Lógica del juego (game_manager) ya estaba iniciada.~n");
        Error -> io:format("ERROR al iniciar game_manager: ~p~n", [Error])
    end,

    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor TCP (Puente) iniciado en el puerto ~p~n", [?PORT]),

    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Conexión aceptada...~n"),

    inet:setopts(Socket, [{packet, 4}]),

    spawn(fun() -> client_handler(Socket) end),
    
    accept_loop(ListenSocket). 

    client_handler(Socket) ->
        %% --- HANDSHAKE DE LOGIN (CORREGIDO) ---
        %% CAMBIO IMPORTANTE: Usamos 'infinity' en vez de 0.
        %% Esperamos pacientemente a que el cliente envíe su nombre.
    case gen_tcp:recv(Socket, 0, infinity) of
        {ok, Data} ->
            try jsx:decode(Data, [return_maps]) of
                #{<<"action">> := <<"join">>, <<"nickname">> := NicknameBin} ->
                    Nickname = binary_to_list(NicknameBin),
                    
                    case game_manager:add_player(self(), Nickname) of
                        ok ->
                            io:format("Jugador ~s (~p) unido.~n", [Nickname, self()]),
                            WelcomeMap = #{
                                action => <<"welcome">>,
                                my_pid => list_to_binary(pid_to_list(self()))
                            },
                            send_json(Socket, WelcomeMap),
                            io:format("~p: [BRIDGE] 'welcome' enviado. Entrando en client_loop...~n", [self()]),
                            client_loop(Socket);
                            
                        {error, Reason} ->
                            io:format("Rechazado: ~p.~n", [Reason]),
                            ErrorMap = #{action => <<"error">>, message => atom_to_binary(Reason, utf8)},
                            send_json(Socket, ErrorMap),
                            gen_tcp:close(Socket)
                    end;
                _ ->
                    io:format("Primer mensaje inválido (se esperaba 'join'). Cerrando.~n"),
                    gen_tcp:close(Socket)
            catch _:_ -> 
                io:format("JSON inválido en handshake.~n"),
                gen_tcp:close(Socket)
            end;
        {error, Reason} ->
            io:format("Error recibiendo handshake: ~p. Cerrando.~n", [Reason]),
            gen_tcp:close(Socket)
    end.

client_loop(Socket) ->
    receive
        {send_json_payload, JsonMap} ->
            send_json(Socket, JsonMap);

        UnexpectedMessage ->
            io:format("~p: [BRIDGE] Recibido mensaje INESPERADO: ~p~n", [self(), UnexpectedMessage])
    after 0 ->
        ok
    end,

    %% Aquí seguimos usando timeout 0 para hacer polling rápido y revisar el buzón (receive)
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Data} ->
            handle_json_from_client(Data, Socket),
            client_loop(Socket);
        
        {error, timeout} ->
            client_loop(Socket);

        {error, closed} ->
            io:format("~p: [BRIDGE] Cliente desconectado (recv closed).~n", [self()]);
            
        {error, Reason} ->
            io:format("~p: [BRIDGE] Error de socket (recv): ~p.~n", [self(), Reason])
    end.

handle_json_from_client(Data, Socket) ->
    MyPID = self(), 
    io:format("~p: [BRIDGE] Recibido JSON crudo: ~p~n", [MyPID, Data]),
    try jsx:decode(Data, [return_maps]) of
        #{<<"action">> := <<"start_game">>} ->
            io:format("~p: [BRIDGE] JSON Decodificado: START_GAME.~n", [MyPID]),
            Result = game_manager:start_game(),
            case Result of
                ok -> send_json(Socket, #{action => <<"start_ok">>});
                {error, Reason} -> send_json(Socket, #{action => <<"start_error">>, reason => Reason})
            end;

        #{<<"action">> := <<"discard_cards">>, <<"cards">> := CardMapsList} ->
            io:format("~p: [BRIDGE] JSON Decodificado: DISCARD_CARDS.~n", [MyPID]),
            CardsToDiscard = lists:map(
                fun(CardMap) ->
                    #card{
                        type = binary_to_atom(maps:get(<<"type">>, CardMap), utf8),
                        color = binary_to_atom(maps:get(<<"color">>, CardMap), utf8),
                        name = binary_to_atom(maps:get(<<"name">>, CardMap), utf8)
                    }
                end, CardMapsList),

            Result = game_manager:discard_cards(MyPID, CardsToDiscard),
            case Result of
                {ok, _} -> send_json(Socket, #{action => <<"discard_ok">>});
                {error, Reason} -> send_json(Socket, #{action => <<"discard_error">>, reason => Reason})
            end;

        #{<<"action">> := <<"play_card">>,
          <<"target_pid">> := TargetPIDBin, 
          <<"card">> := CardMap,             
          <<"player_color">> := PlayerColorBin, 
          <<"target_color">> := TargetColorBin} ->
            
            TargetPID = list_to_pid(binary_to_list(TargetPIDBin)),
            Card = #card{
                type = binary_to_atom(maps:get(<<"type">>, CardMap), utf8),
                color = binary_to_atom(maps:get(<<"color">>, CardMap), utf8),
                name = binary_to_atom(maps:get(<<"name">>, CardMap), utf8)
            },
            PlayerColor = binary_to_atom(PlayerColorBin, utf8),
            TargetColor = binary_to_atom(TargetColorBin, utf8),

            io:format("~p: Cliente pide jugar ~p en ~p (target: ~p)~n", 
                [MyPID, Card#card.name, TargetColor, TargetPID]),

            Result = game_manager:play_card(MyPID, TargetPID, Card, PlayerColor, TargetColor),
            case Result of
                {ok, _} -> send_json(Socket, #{action => <<"play_ok">>});
                {error, Reason} -> send_json(Socket, #{action => <<"play_error">>, reason => Reason})
            end;

        #{<<"action">> := <<"contagion_step">>,
          <<"target_pid">> := TargetPIDBin,
          <<"color">> := ColorBin,
          <<"source_color">> := SourceColorBin} ->
            
            TargetPID = list_to_pid(binary_to_list(TargetPIDBin)),
            Color = binary_to_atom(ColorBin, utf8),
            SourceColor = binary_to_atom(SourceColorBin, utf8),
            
            io:format("~p: Paso de contagio -> Target: ~p Color: ~p~n", [MyPID, TargetPID, Color]),
            
            Result = game_manager:contagion_step(MyPID, TargetPID, SourceColor, Color),
            
            case Result of
                {ok, contagion_continue} -> send_json(Socket, #{action => <<"contagion_continue">>});
                {ok, turn_finished} -> send_json(Socket, #{action => <<"contagion_finished">>}); 
                {ok, _} -> send_json(Socket, #{action => <<"update_ok">>});
                {error, Reason} -> send_json(Socket, #{action => <<"play_error">>, reason => Reason})
            end;

        Other ->
            io:format("~p: JSON no reconocido: ~p~n", [MyPID, Other])

    catch
        _:Error ->
            io:format("~p: Error de JSON: ~p. Datos: ~p~n", [MyPID, Error, Data])
    end.

send_json(Socket, ErlangMap) ->
    try 
        Payload = jsx:encode(ErlangMap),
        gen_tcp:send(Socket, Payload)
    catch
        _:Error ->
            io:format("Error al enviar JSON: ~p~n", [Error])
    end.