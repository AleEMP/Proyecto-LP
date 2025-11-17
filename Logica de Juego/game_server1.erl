-module(game_server).
-behaviour(gen_server).

% API pública
-export([
    start_link/0,
    add_player/1,
    play_card/5,
    start_game/0
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    initial_board/0,
    deal_initial_hands_and_boards/3,
    start_game/2,
    logic_start_turn/2
]).

-include("virus_defs.hrl"). 

% Asumimos un mínimo de 2 jugadores para iniciar.


% --- Estructura del Estado del Servidor ---
-record(state, {
    deck = [],
    discard_pile = [],
    players = [],
    player_hands = #{},
    % player_boards ahora es: PID -> #{Color => #organ_slot{state=N, cards=[...]} }
    player_boards = #{}, 
    
    % NUEVO: PID del jugador actual
    current_player = undefined, 
    
    % NUEVO: El estado interno del juego (e.g., waiting_for_players, turn_in_progress)
    game_stage = waiting_for_players
}).

%% ----------------------------------------------------
%% API (Funciones Públicas)
%% ----------------------------------------------------

start_link() ->
    gen_server:start_link({local, game_manager}, ?MODULE, [], []).

add_player(PlayerPID) ->
    gen_server:call(game_manager, {add_player, PlayerPID}).

play_card(PlayerPID, TargetPID, Card, PlayerColor, TargetColor) ->
    gen_server:call(game_manager, {play, PlayerPID, TargetPID, Card, PlayerColor, TargetColor}).

start_game() -> 
    gen_server:call(game_manager, start_game).

%% ----------------------------------------------------
%% Lógica de Soporte de Juego
%% ----------------------------------------------------

%% @doc Define el estado inicial de un tablero de jugador: 4 órganos a 0 (vacío).
initial_board() ->
    % 2. ACTUALIZACIÓN: Ahora se inicializa con el registro #organ_slot{}
    lists:foldl(fun(Color, Map) -> 
        maps:put(Color, #organ_slot{}, Map) % Inicializa con un slot vacío (state=0, cards=[])
    end, #{}, ?ORGAN_COLORS).

%% @doc Reparte cartas a todos los jugadores y crea sus tableros iniciales, usando draw_cards/3.
%% NOTA: Se ha cambiado a deal_initial_hands_and_boards/3
%% Retorna: {RemainingDeck, FinalDiscardPile, NewPlayerHands, InitialBoards}
deal_initial_hands_and_boards(Players, FullDeck, InitialDiscard) ->
    CardsPerPlayer = 3, % La mano inicial es de 3 cartas
    
    % Inicializamos el acumulador: {MazoActual, DescarteActual, ManosActuales}
    InitialAcc = {FullDeck, InitialDiscard, #{}},
    
    {DeckAfterDeal, DiscardAfterDeal, PlayerHands} = lists:foldl(
        fun(PlayerPID, {AccDeck, AccDiscard, AccHands}) ->
            % USAMOS game_model:draw_cards/3 para robar 3 cartas
            {RemainingDeck, Hand, NewDiscard} = game_model:draw_cards(CardsPerPlayer, AccDeck, AccDiscard),
            
            NewAccHands = maps:put(PlayerPID, Hand, AccHands),
            
            {RemainingDeck, NewDiscard, NewAccHands}
        end, InitialAcc, Players),
    
    % Asigna un tablero vacío a cada jugador
    InitialBoards = maps:from_list(lists:map(
        fun(PlayerPID) -> {PlayerPID, initial_board()} end, Players)),
    
    % Retornamos también la pila de descarte actualizada.
    {DeckAfterDeal, DiscardAfterDeal, PlayerHands, InitialBoards}.

%% ----------------------------------------------------
%% Lógica de Inicio de Juego (Reparto)
%% ----------------------------------------------------

start_game(Players, State) ->
    io:format("--- Iniciando partida, repartiendo a ~w jugadores ---~n", [length(Players)]),
    
    % Implementación de la lógica de reparto (USANDO LA FUNCIÓN MODIFICADA)
    {RemainingDeck, FinalDiscardPile, NewPlayerHands, InitialBoards} = 
        deal_initial_hands_and_boards(Players, State#state.deck, State#state.discard_pile),
    
    % Elegir el primer jugador (el primero en la lista, por ejemplo)
    FirstPlayer = hd(Players), 
    
    io:format("Juego listo. Quedan ~w cartas en el mazo. El primer turno es para ~w.~n", 
              [length(RemainingDeck), FirstPlayer]),
    
    % Retorna el nuevo estado
    NewState = State#state{
        deck = RemainingDeck,
        discard_pile = FinalDiscardPile, % <-- IMPORTANTE: Actualizar el descarte
        player_hands = NewPlayerHands,
        player_boards = InitialBoards,
        players = Players, % Asegurarse de que la lista esté guardada
        current_player = FirstPlayer, % Establecer el jugador inicial
        game_stage = turn_in_progress
    },
    
    % **Mecanismo de Bucle:** Enviar un mensaje interno para iniciar el primer turno.
    gen_server:cast(self(), {start_turn_process, FirstPlayer}), 
    
    NewState. % Solo retorna el estado modificado.

%% ----------------------------------------------------
%% Lógica de Jugada (Manejo de Transición de Estados y Validación)
%% ----------------------------------------------------

logic_start_turn(PlayerPID, State) ->
    PlayerHand = maps:get(PlayerPID, State#state.player_hands),
    Deck = State#state.deck,
    DiscardPile = State#state.discard_pile,
    
    HandLimit = 3, % Asumimos 3 cartas como límite/mano inicial

    case length(PlayerHand) of
        0 ->
            % Se activa el efecto de "turno perdido" por Guante de Látex o descarte completo.
            % Robar la mano inicial (3 cartas).
            CardsToDraw = HandLimit, 
            {NewDeck, DrawnCards, NewDiscardPile} = game_model:draw_cards(CardsToDraw, Deck, DiscardPile),
            
            io:format(" -> Jugador ~w comienza turno con mano vacía. Roba ~w cartas.~n", [PlayerPID, CardsToDraw]),

            % Actualizar el estado con las cartas robadas
            NewState = State#state{
                deck = NewDeck,
                discard_pile = NewDiscardPile,
                player_hands = maps:put(PlayerPID, DrawnCards, State#state.player_hands)
            },
            
            % Después de robar, el turno continúa con la fase de acción (devolver el estado)
            {NewState, {action_phase, PlayerPID}}; 
            
        _ ->
            % Turno normal. Se procede directamente a la fase de acción.
            {State, {action_phase, PlayerPID}}
    end.

logic_end_turn(PlayerPID, State) ->
    % El estado 'State' ya incluye el robo de 1 carta de reemplazo realizado en handle_call
    
    % 1. Verificar condición de victoria antes de pasar el turno
    case game_model:check_win_condition(State) of
        {won, WinnerPID} ->
            io:format(" -> ¡VICTORIA! El jugador ~w ha ganado la partida.~n", [WinnerPID]),
            {State, {game_over, WinnerPID}};
        
        no_winner ->
            % 2. Pasar al siguiente jugador
            PlayerPIDs = State#state.players,
            NextPlayerPID = game_model:next_player(PlayerPID, PlayerPIDs),
            
            io:format(" -> Turno finalizado. El siguiente jugador es ~w.~n", [NextPlayerPID]),
            
            % Retornar el estado final y el PID del siguiente jugador para el bucle del servidor
            {State, {next_turn, NextPlayerPID}}
    end.


%% Retorna: {NewState, {ok, played, CardsToDiscardFromLogic} | {error, Reason}}
apply_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State) -> 
    PlayerBoards = State#state.player_boards, 

    case Card#card.type of
        ?T_TREATMENT ->
            % apply_treatment_move retorna {NewState, IsSuccessful, CardsToDiscardFromLogic}
            {NewState, IsSuccessful, CardsToDiscardFromLogic} = 
                apply_treatment_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State),
            
            if 
                IsSuccessful -> {NewState, {ok, played, CardsToDiscardFromLogic}};
                true -> {State, {error, invalid_treatment_target}} 
            end;

        _ -> 
            % 1. Lógica para Organo/Virus/Medicina: 
            TargetBoard = maps:get(TargetPID, PlayerBoards),
            
            % El modelo DEBE retornar {ok, Color, NewBoard, CardsToDiscardFromBoard}
            ValidationResult = game_model:apply_card_to_board(Card, TargetColor, TargetBoard), 

            case ValidationResult of
                % El modelo retorna el nuevo tablero Y la lista de cartas extirpadas/curadas
                {ok, _ValidColor, NewBoard, CardsToDiscardFromBoard} -> 
                    io:format("  -> Órgano afectado: ~p. Jugada válida en el cuerpo de ~p.~n", [TargetColor, TargetPID]),
                    
                    % SOLO actualizar el tablero. El descarte y robo se hace en handle_call.
                    NewPlayerBoards = maps:put(TargetPID, NewBoard, PlayerBoards),

                    NewState = State#state{
                        player_boards = NewPlayerBoards 
                    },
                    
                    % Retornar las cartas que deben ir al descarte (aparte de la carta jugada)
                    {NewState, {ok, played, CardsToDiscardFromBoard}};

                % --- JUGADA INVÁLIDA ---
                {error, Reason} ->
                    io:format("  -> ERROR: Jugada inválida. Razón: ~p. Estado sin cambios.~n", [Reason]),
                    {State, {error, Reason}}
            end
    end.


%% @doc Aplica la lógica de las cartas de Tratamiento al estado global del juego.
%% Retorna: {NewState, IsSuccessful::boolean(), CardsToDiscardFromLogic}
apply_treatment_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State) ->
    CardName = Card#card.name,
    
    {NewState, IsSuccessful, CardsToDiscard} = 
        case CardName of
            ?N_TRANSPLANT -> 
                {NState, IsS} = logic_transplant(PlayerPID, TargetPID, PlayerColor, TargetColor, State),
                {NState, IsS, []};
                
            ?N_ORGAN_THIEF ->
                % TargetArg es el Color a robar
                {NState, IsS} = logic_organ_thief(PlayerPID, TargetPID, TargetColor, State),
                {NState, IsS, []};
                
            %?N_CONTAGION ->
                % TargetArg es la lista de transferencias
            %    {NState, IsS} = logic_contagion(PlayerPID, State), %% REVISAR, ESTÁ HASTA LAS WEBAS 
            %    {NState, IsS, []};
                
            ?N_LATEX_GLOVE ->
                % La lógica de Latex devuelve las cartas descartadas
                {StateAfterGlove, DiscardedHands} = logic_latex_glove(PlayerPID, State),
                {StateAfterGlove, true, DiscardedHands};
                
            ?N_MEDICAL_MISTAKE ->
                % TargetPID es el jugador cuyo tablero se intercambia
                {NState, IsS} = logic_medical_error(PlayerPID, TargetPID, State),
                {NState, IsS, []};
                
            _ ->
                {State, false, []} % Tratamiento desconocido o no implementado
        end,
    
    {NewState, IsSuccessful, CardsToDiscard}.

logic_transplant(PlayerPID, TargetPID, PlayerColor, TargetColor, State) ->
    Boards = State#state.player_boards,
    PlayerBoard = maps:get(PlayerPID, Boards),
    TargetBoard = maps:get(TargetPID, Boards),
    
    PSlot = maps:get(PlayerColor, PlayerBoard),
    TSlot = maps:get(TargetColor, TargetBoard),

    % 1. Validación de Inmunidad
    IsImmune = PSlot#organ_slot.state == 3 orelse TSlot#organ_slot.state == 3,
    
    % 2. VALIDACIÓN DE DUPLICIDAD (Corregida la sintaxis de acceso al registro)
    % P está recibiendo un órgano de color TargetColor. Falla si P ya tiene un TargetColor.
    IsPlayerDuplicate = (maps:get(TargetColor, PlayerBoard))#organ_slot.state /= 0,
    
    % T está recibiendo un órgano de color PlayerColor. Falla si T ya tiene un PlayerColor.
    IsTargetDuplicate = (maps:get(PlayerColor, TargetBoard))#organ_slot.state /= 0,
    
    if
        IsImmune orelse IsPlayerDuplicate orelse IsTargetDuplicate ->
            % La jugada es inválida
            {State, false};
            
        true ->
            
            EmptySlot = #organ_slot{state = 0, cards = []},
            
            NewPlayerBoard = maps:put(TargetColor, TSlot, 
                maps:put(PlayerColor, EmptySlot, PlayerBoard)), 
            NewTargetBoard = maps:put(PlayerColor, PSlot,
                maps:put(TargetColor, EmptySlot, TargetBoard)), 
            
            NewBoards = maps:put(PlayerPID, NewPlayerBoard, 
                                 maps:put(TargetPID, NewTargetBoard, Boards)),
            
            {State#state{player_boards = NewBoards}, true}
    end.

logic_organ_thief(PlayerPID, TargetPID, Color, State) ->
    % 1. Definición de colores y obtención de slots
    Boards = State#state.player_boards,
    PlayerBoard = maps:get(PlayerPID, Boards),
    TargetBoard = maps:get(TargetPID, Boards),
    
    TSlot = maps:get(Color, TargetBoard), % Contenido del slot a ROBAR (del oponente)

    % 2. VALIDACIONES
    
    % 2a. Inmunidad del Oponente: No se puede robar si el slot del oponente está INMUNE (state = 3).
    IsImmune = TSlot#organ_slot.state == 3,
    
    % 2b. Duplicidad del Jugador: El jugador no puede robar si ya tiene un órgano de ese color. (Corregida sintaxis)
    IsPlayerDuplicate = (maps:get(Color, PlayerBoard))#organ_slot.state /= 0,
    
    % 2c. Slot Oponente Vacío: No se puede robar un slot que ya está vacío.
    IsTargetEmpty = TSlot#organ_slot.state == 0,
    
    if
        IsImmune orelse IsPlayerDuplicate or IsTargetEmpty ->
            % La jugada es inválida
            {State, false};
            
        true ->
            % 3. EJECUCIÓN DEL ROBO
            EmptySlot = #organ_slot{state = 0, cards = []},
            
            % 3a. TABLERO JUGADOR: El slot del Jugador (PColor/TColor) recibe el contenido robado (TSlot)
            NewPlayerBoard = maps:put(Color, TSlot, PlayerBoard), 
            
            % 3b. TABLERO OPONENTE: El slot donado (TColor) se vacía.
            NewTargetBoard = maps:put(Color, EmptySlot, TargetBoard), 
            
            % 4. ACTUALIZACIÓN GLOBAL
            NewBoards = maps:put(PlayerPID, NewPlayerBoard, 
                                 maps:put(TargetPID, NewTargetBoard, Boards)),
            
            {State#state{player_boards = NewBoards}, true}
    end.



%% @doc Descarta las manos de todos los oponentes. Retorna {NewState, DiscardedCardsList}
logic_latex_glove(PlayerPID, State) ->
    % 1. Obtener los datos del estado
    AllHands = State#state.player_hands,
    % CurrentDiscard = State#state.discard_pile, % Ya no es necesario aquí, se gestiona en handle_call

    % 2. Recorrer todas las manos para descartar, excluyendo al jugador actual.
    {NewHands, DiscardedCardsList} = maps:fold(
        fun(PID, Hand, {AccHands, AccDiscard}) ->
            if PID =/= PlayerPID ->
                % Es un Oponente: Su mano se vacía y sus cartas van a la lista de descarte temporal.
                NewAccHands = maps:put(PID, [], AccHands),
                NewAccDiscard = Hand ++ AccDiscard,
                {NewAccHands, NewAccDiscard};
            true ->
                % Es el Jugador que lanzó la carta: Su mano se mantiene intacta.
                {AccHands, AccDiscard}
            end
        end, {AllHands, []}, AllHands),

    % 3. Crear el nuevo estado (La gestión del descarte final se hace en apply_treatment_move/handle_call)
    NewState = State#state{
        player_hands = NewHands
        % discard_pile se actualiza en handle_call para incluir la carta de guante de látex
    },

    % Retorna el estado modificado y la lista de cartas descartadas de las manos.
    {NewState, DiscardedCardsList}.


logic_medical_error(PlayerPID, TargetPID, State) ->
    AllBoards = State#state.player_boards,
    
    % 1. Obtener los tableros que serán intercambiados
    PlayerBoard = maps:get(PlayerPID, AllBoards), % Tablero del jugador que lanza la carta
    TargetBoard = maps:get(TargetPID, AllBoards), % Tablero del jugador objetivo
    
    % 2. Intercambiar los tableros en el mapa global de player_boards
    
    % Paso A: El PID del jugador ahora apunta al tablero original del oponente
    BoardsStep1 = maps:put(PlayerPID, TargetBoard, AllBoards),
    
    % Paso B: El PID del oponente ahora apunta al tablero original del jugador
    BoardsStep2 = maps:put(TargetPID, PlayerBoard, BoardsStep1),
    
    % 3. Actualizar el estado con el nuevo mapa de tableros
    NewState = State#state{player_boards = BoardsStep2},
    
    % La jugada de tratamiento es exitosa
    {NewState, true}.





%% ----------------------------------------------------
%% gen_server Callbacks (Lógica Interna)
%% ----------------------------------------------------

init([]) ->
    io:format("~p: Game Manager iniciando...~n", [self()]),
    FullDeck = game_model:create_and_shuffle_deck(),
    {ok, #state{deck = FullDeck}}.

%% ----------------------------------------------------
%% handle_call/3 (TODAS JUNTAS)
%% ----------------------------------------------------

handle_call({add_player, PlayerPID}, _From, State) ->
    Players = State#state.players,
    CurrentCount = length(Players),

    if CurrentCount >= ?MAX_PLAYERS ->
        io:format(" -> ERROR: El juego ha alcanzado el límite máximo de ~w jugadores.~n", [?MAX_PLAYERS]),
        {reply, {error, max_players_reached}, State};
    State#state.game_stage /= waiting_for_players ->
        {reply, {error, game_already_in_progress}, State};
    true ->
        io:format("Jugador ~p unido al juego (~w/~w).~n", [PlayerPID, CurrentCount + 1, ?MAX_PLAYERS]),
        NewPlayers = Players ++ [PlayerPID],
        NewState = State#state{players = NewPlayers},
        
        {reply, ok, NewState} 
    end;

handle_call(start_game, _From, State) -> % <-- NUEVA CLÁUSULA
    CurrentPlayers = State#state.players,
    PlayerCount = length(CurrentPlayers),

    if PlayerCount < ?MIN_PLAYERS ->
        {reply, {error, not_enough_players}, State};
    State#state.game_stage == waiting_for_players ->
        io:format(" -> Jugador inició el juego manualmente. Repartiendo...~n", []),
        NewState = start_game(CurrentPlayers, State), % Llama a la lógica de inicio
        {reply, ok, NewState};
    true ->
        {reply, {error, game_already_started}, State}
    end;

handle_call({play, PlayerPID, TargetPID, Card, PlayerColor, TargetColor}, _From, State) ->
    io:format("--- Reporte de Jugada Recibida ---~n"
              "  PID Jugador (Lanzador): ~p~n"
              "  PID Objetivo (Cuerpo): ~p~n"
              "  Carta Jugada: ~p~n"
              "  Color de Referencia del Jugador/Origen: ~p~n"
              "  Color de Objetivo/Destino: ~p~n"
              "-----------------------------------~n", 
              [PlayerPID, TargetPID, Card, PlayerColor, TargetColor]),
    
    % 1. Ejecutar la jugada: Retorna {StateAfterMove, {ok, played, CardsToDiscard} | {error, Reason}}
    {StateAfterMove, Reply} = apply_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State), 
    
    % 2. Verificar si la jugada fue exitosa
    case Reply of
        {ok, _Played, CardsToDiscardFromLogic} ->
            % --- INICIO: GESTIÓN CENTRALIZADA DE LA CARTA JUGADA ---
            
            % 2a. Remover la carta jugada (Card) de la mano.
            PlayerHand = maps:get(PlayerPID, StateAfterMove#state.player_hands),
            NewHand_NoPlayed = lists:delete(Card, PlayerHand), 
            
            % 2b. Mover la carta jugada y las cartas extirpadas a la pila de descarte.
            AllCardsToDiscard = [Card | CardsToDiscardFromLogic],
            NewDiscardPile_Temp = AllCardsToDiscard ++ StateAfterMove#state.discard_pile,

            % 2c. Robar una carta de reemplazo (1 carta).
            Deck = StateAfterMove#state.deck,
            {FinalDeck, [DrawnCard], FinalDiscardPile} = 
                game_model:draw_cards(1, Deck, NewDiscardPile_Temp),

            NewHand = [DrawnCard | NewHand_NoPlayed],
            
            % 2d. Actualizar el estado con los cambios de mano/mazo/descarte
            NewPlayerHands = maps:put(PlayerPID, NewHand, StateAfterMove#state.player_hands),

            StateAfterDraw = StateAfterMove#state{
                player_hands = NewPlayerHands,
                deck = FinalDeck,
                discard_pile = FinalDiscardPile
            },
            PlayerPID ! {draw, DrawnCard}, % 

            % --- FIN: GESTIÓN DE LA CARTA JUGADA ---
            
            % 3. Lógica de fin de turno (SOLO verifica victoria y pasa turno, NO roba)
            {StateFinal, NextAction} = logic_end_turn(PlayerPID, StateAfterDraw),
            
            case NextAction of
                {next_turn, NextPlayerPID} ->
                    % Actualizar el estado con el nuevo jugador
                    NewState = StateFinal#state{
                        current_player = NextPlayerPID,
                        game_stage = waiting_for_player_action
                    },
                    
                    % **Mecanismo de Bucle:** Enviar un mensaje interno (cast) para iniciar el turno.
                    gen_server:cast(self(), {start_turn_process, NextPlayerPID}),
                    
                    {reply, {ok, played_and_next_turn}, NewState};
                    
                {game_over, WinnerPID} ->
                    % El juego termina
                    NewState = StateFinal#state{
                        game_stage = {game_over, WinnerPID}
                    },
                    {reply, {ok, game_over}, NewState}
            end;
            
        {error, _Reason} ->
            % Jugada inválida: el turno NO termina. El cliente debe reintentar.
            {reply, Reply, StateAfterMove}
    end;


handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% ----------------------------------------------------
%% handle_cast/2 (TODAS JUNTAS)
%% ----------------------------------------------------

%% @doc Maneja el mensaje para comenzar el proceso de un turno.
handle_cast({start_turn_process, PlayerPID}, State) ->
    % 1. Lógica de inicio de turno (Ej: robar si mano vacía)
    {StateAfterStart, {action_phase, _}} = logic_start_turn(PlayerPID, State),
    
    % 2. Notificar al jugador que es su turno (para que el cliente envíe 'play_card')
    PlayerPID ! {your_turn},
    
    % 3. Registrar la etapa del juego
    NewState = StateAfterStart#state{game_stage = {action_phase, PlayerPID}},
    
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% ----------------------------------------------------
%% handle_info/2
%% ----------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%% ----------------------------------------------------
%% terminate/2 y code_change/3
%% ----------------------------------------------------

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.