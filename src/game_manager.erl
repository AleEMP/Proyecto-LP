-module(game_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    add_player/2,
    play_card/5,
    discard_cards/2,
    start_game/0,
    contagion_step/4
]).

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

-record(state, {
    deck = [],
    discard_pile = [],
    players = [],
    nicknames = #{},
    player_hands = #{},
    player_boards = #{}, 
    current_player = undefined, 
    game_stage = waiting_for_players
}).

start_link() ->
    gen_server:start_link({local, game_manager}, game_manager, [], []).

add_player(PlayerPID, Nickname) ->
    gen_server:call(game_manager, {add_player, PlayerPID, Nickname}).

play_card(PlayerPID, TargetPID, Card, PlayerColor, TargetColor) ->
    gen_server:call(game_manager, {play, PlayerPID, TargetPID, Card, PlayerColor, TargetColor}).

start_game() -> 
    gen_server:call(game_manager, start_game).

discard_cards(PlayerPID, Cards) ->
    gen_server:call(game_manager, {discard_cards, PlayerPID, Cards}).

contagion_step(PlayerPID, TargetPID, SourceColor, Color) ->
    gen_server:call(game_manager, {contagion_step, PlayerPID, TargetPID, SourceColor, Color}).

initial_board() ->
    lists:foldl(fun(Color, Map) -> 
        maps:put(Color, #organ_slot{}, Map) 
    end, #{}, ?ORGAN_COLORS).

deal_initial_hands_and_boards(Players, FullDeck, InitialDiscard) ->
    CardsPerPlayer = 3, 
    InitialAcc = {FullDeck, InitialDiscard, #{}},
    {DeckAfterDeal, DiscardAfterDeal, PlayerHands} = lists:foldl(
        fun(PlayerPID, {AccDeck, AccDiscard, AccHands}) ->
            {RemainingDeck, Hand, NewDiscard} = game_model:draw_cards(CardsPerPlayer, AccDeck, AccDiscard),
            NewAccHands = maps:put(PlayerPID, Hand, AccHands),
            {RemainingDeck, NewDiscard, NewAccHands}
        end, InitialAcc, Players),
    InitialBoards = maps:from_list(lists:map(
        fun(PlayerPID) -> {PlayerPID, initial_board()} end, Players)),
    {DeckAfterDeal, DiscardAfterDeal, PlayerHands, InitialBoards}.

start_game(Players, State) ->
    io:format("--- Iniciando partida, repartiendo a ~w jugadores ---~n", [length(Players)]),
    {RemainingDeck, FinalDiscardPile, NewPlayerHands, InitialBoards} = 
        deal_initial_hands_and_boards(Players, State#state.deck, State#state.discard_pile),
    FirstPlayer = hd(Players), 
    io:format("Juego listo. Quedan ~w cartas en el mazo. El primer turno es para ~w.~n", 
              [length(RemainingDeck), FirstPlayer]),
    NewState = State#state{
        deck = RemainingDeck,
        discard_pile = FinalDiscardPile, 
        player_hands = NewPlayerHands,
        player_boards = InitialBoards,
        players = Players, 
        current_player = FirstPlayer, 
        game_stage = turn_in_progress
    },
    broadcast_state(NewState),
    gen_server:cast(self(), {start_turn_process, FirstPlayer}), 
    NewState. 

logic_start_turn(PlayerPID, State) ->
    PlayerHand = maps:get(PlayerPID, State#state.player_hands),
    Deck = State#state.deck,
    DiscardPile = State#state.discard_pile,
    HandLimit = 3, 

    case length(PlayerHand) of
        0 ->
            CardsToDraw = HandLimit, 
            {NewDeck, DrawnCards, NewDiscardPile} = game_model:draw_cards(CardsToDraw, Deck, DiscardPile),
            io:format(" -> Jugador ~w comienza turno con mano vacía. Roba ~w cartas.~n", [PlayerPID, CardsToDraw]),
            NewState = State#state{
                deck = NewDeck,
                discard_pile = NewDiscardPile,
                player_hands = maps:put(PlayerPID, DrawnCards, State#state.player_hands)
            },
            {NewState, {end_turn_after_draw, PlayerPID}}; 
        _ ->
            {State, {action_phase, PlayerPID}}
    end.

logic_end_turn(PlayerPID, State) ->
    case game_model:check_win_condition(State#state.player_boards) of
        {won, WinnerPID} ->
            io:format(" -> ¡VICTORIA! El jugador ~w ha ganado la partida.~n", [WinnerPID]),
            {State, {game_over, WinnerPID}};
        no_winner ->
            PlayerPIDs = State#state.players,
            NextPlayerPID = game_model:next_player(PlayerPID, PlayerPIDs),
            io:format(" -> Turno finalizado. El siguiente jugador es ~w.~n", [NextPlayerPID]),
            {State, {next_turn, NextPlayerPID}}
    end.

apply_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State) -> 
    PlayerBoards = State#state.player_boards, 
    case Card#card.type of
        ?T_TREATMENT ->
            {NewState, IsSuccessful, CardsToDiscardFromLogic} = 
                apply_treatment_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State),
            FinalCardsToDiscard = [Card | CardsToDiscardFromLogic],
            if 
                IsSuccessful -> {NewState, {ok, played, FinalCardsToDiscard}}; 
                true -> {State, {error, invalid_treatment_target}} 
            end;
        _ -> 
            TargetBoard = maps:get(TargetPID, PlayerBoards),
            ValidationResult = game_model:apply_card_to_board(Card, TargetColor, TargetBoard), 
            case ValidationResult of
                {ok, _ValidColor, NewBoard, CardsToDiscardFromBoard} -> 
                    io:format("  -> Órgano afectado: ~p. Jugada válida en el cuerpo de ~p.~n", [TargetColor, TargetPID]),
                    NewPlayerBoards = maps:put(TargetPID, NewBoard, PlayerBoards),
                    NewState = State#state{
                        player_boards = NewPlayerBoards 
                    },
                    {NewState, {ok, played, CardsToDiscardFromBoard}};
                {error, Reason} ->
                    io:format("  -> ERROR: Jugada inválida. Razón: ~p. Estado sin cambios.~n", [Reason]),
                    {State, {error, Reason}}
            end
    end.

apply_treatment_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State) ->
    CardName = Card#card.name,
    {NewState, IsSuccessful, CardsToDiscard} = 
        case CardName of
            ?N_TRANSPLANT -> 
                {NState, IsS} = logic_transplant(PlayerPID, TargetPID, PlayerColor, TargetColor, State),
                {NState, IsS, []};
            ?N_ORGAN_THIEF ->
                {NState, IsS} = logic_organ_thief(PlayerPID, TargetPID, TargetColor, State),
                {NState, IsS, []};
            ?N_CONTAGION ->
                {State, false, []}; 
            ?N_LATEX_GLOVE ->
                {StateAfterGlove, DiscardedHands} = logic_latex_glove(PlayerPID, State),
                {StateAfterGlove, true, DiscardedHands};
            ?N_MEDICAL_MISTAKE ->
                {NState, IsS} = logic_medical_error(PlayerPID, TargetPID, State),
                {NState, IsS, []};
            _ ->
                {State, false, []} 
        end,
    {NewState, IsSuccessful, CardsToDiscard}.

logic_transplant(PlayerPID, TargetPID, PlayerColor, TargetColor, State) ->
    Boards = State#state.player_boards,
    PlayerBoard = maps:get(PlayerPID, Boards),
    TargetBoard = maps:get(TargetPID, Boards),
    
    PSlot = maps:get(PlayerColor, PlayerBoard),
    TSlot = maps:get(TargetColor, TargetBoard),

    IsEmptySlot = PSlot#organ_slot.state == 0 orelse TSlot#organ_slot.state == 0,
    IsImmune = PSlot#organ_slot.state == 3 orelse TSlot#organ_slot.state == 3,
    IsSmameColor = PlayerColor == TargetColor,
    IsPlayerDuplicate = (maps:get(TargetColor, PlayerBoard))#organ_slot.state /= 0,
    IsTargetDuplicate = (maps:get(PlayerColor, TargetBoard))#organ_slot.state /= 0,

    if
        IsEmptySlot orelse IsImmune orelse (not IsSmameColor andalso (IsPlayerDuplicate orelse IsTargetDuplicate)) ->
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
    Boards = State#state.player_boards,
    PlayerBoard = maps:get(PlayerPID, Boards),
    TargetBoard = maps:get(TargetPID, Boards),
    TSlot = maps:get(Color, TargetBoard), 
    IsImmune = TSlot#organ_slot.state == 3,
    IsPlayerDuplicate = (maps:get(Color, PlayerBoard))#organ_slot.state /= 0,
    IsTargetEmpty = TSlot#organ_slot.state == 0,
    if
        IsImmune orelse IsPlayerDuplicate orelse IsTargetEmpty ->
            {State, false};
        true ->
            EmptySlot = #organ_slot{state = 0, cards = []},
            NewPlayerBoard = maps:put(Color, TSlot, PlayerBoard), 
            NewTargetBoard = maps:put(Color, EmptySlot, TargetBoard), 
            NewBoards = maps:put(PlayerPID, NewPlayerBoard, 
                                 maps:put(TargetPID, NewTargetBoard, Boards)),
            {State#state{player_boards = NewBoards}, true}
    end.

logic_latex_glove(PlayerPID, State) ->
    AllHands = State#state.player_hands,
    {NewHands, DiscardedCardsList} = maps:fold(
        fun(PID, Hand, {AccHands, AccDiscard}) ->
            if PID =/= PlayerPID ->
                NewAccHands = maps:put(PID, [], AccHands),
                NewAccDiscard = Hand ++ AccDiscard,
                {NewAccHands, NewAccDiscard};
            true ->
                {AccHands, AccDiscard}
            end
        end, {AllHands, []}, AllHands),
    NewState = State#state{
        player_hands = NewHands
    },
    {NewState, DiscardedCardsList}.

logic_medical_error(PlayerPID, TargetPID, State) ->
    AllBoards = State#state.player_boards,
    PlayerBoard = maps:get(PlayerPID, AllBoards),
    TargetBoard = maps:get(TargetPID, AllBoards), 
    BoardsStep1 = maps:put(PlayerPID, TargetBoard, AllBoards),
    BoardsStep2 = maps:put(TargetPID, PlayerBoard, BoardsStep1),
    NewState = State#state{player_boards = BoardsStep2},
    {NewState, true}.

init([]) ->
    io:format("~p: Game Manager iniciando...~n", [self()]),
    FullDeck = game_model:create_and_shuffle_deck(),
    {ok, #state{deck = FullDeck}}.

handle_call({add_player, PlayerPID, Nickname}, _From, State) ->
    Players = State#state.players,
    CurrentCount = length(Players),
    if CurrentCount >= ?MAX_PLAYERS ->
        io:format(" -> ERROR: El juego ha alcanzado el límite máximo de ~w jugadores.~n", [?MAX_PLAYERS]),
        {reply, {error, max_players_reached}, State};
    State#state.game_stage /= waiting_for_players ->
        {reply, {error, game_already_in_progress}, State};
    true ->
        io:format("Jugador ~s (~p) unido al juego (~w/~w).~n", [Nickname, PlayerPID, CurrentCount + 1, ?MAX_PLAYERS]),
        NewPlayers = Players ++ [PlayerPID],
        
        NewNicknames = maps:put(PlayerPID, list_to_binary(Nickname), State#state.nicknames),
        NewState = State#state{players = NewPlayers, nicknames = NewNicknames},
        {reply, ok, NewState} 
    end;

handle_call(start_game, _From, State) -> 
    io:format("~p: [LOGIC] handle_call(start_game) recibido.~n", [self()]),
    CurrentPlayers = State#state.players,
    PlayerCount = length(CurrentPlayers),
    if PlayerCount < ?MIN_PLAYERS ->
        {reply, {error, not_enough_players}, State};
    State#state.game_stage == waiting_for_players ->
        io:format(" -> Jugador inició el juego manualmente. Repartiendo...~n", []),
        NewState = start_game(CurrentPlayers, State), 
        {reply, ok, NewState};
    true ->
        {reply, {error, game_already_started}, State}
    end;

handle_call({play, PlayerPID, TargetPID, Card, PlayerColor, TargetColor}, _From, State) ->
    io:format("--- Reporte de Jugada Recibida: ~p ---~n", [Card#card.name]),
    case Card#card.name of
        ?N_CONTAGION ->
            handle_contagion_start(PlayerPID, Card, State);
        _ ->
            process_normal_play(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State)
    end;

handle_call({contagion_step, PlayerPID, TargetPID, SourceColor, Color}, _From, State) ->
    if State#state.game_stage /= contagion_phase ->
         {reply, {error, not_in_contagion_phase}, State};
       State#state.current_player /= PlayerPID ->
         {reply, {error, not_your_turn}, State};
       true ->
         case game_model:move_virus_card(PlayerPID, TargetPID, SourceColor, Color, State#state.player_boards) of
             {ok, NewBoards} ->
                 io:format(" -> [CONTAGIO] Virus movido de ~p (~p) a ~p (~p).~n", 
                           [SourceColor, PlayerPID, Color, TargetPID]),
                 StateUpdated = State#state{player_boards = NewBoards},
                 broadcast_state(StateUpdated),
                 case game_model:can_contagion_happen(PlayerPID, NewBoards) of
                     true ->
                         io:format(" -> [CONTAGIO] Aún hay movimientos posibles. Continuando...~n"),
                         {reply, {ok, contagion_continue}, StateUpdated};
                     false ->
                         io:format(" -> [CONTAGIO] No hay más movimientos. Terminando turno.~n"),
                         finish_turn_flow(PlayerPID, StateUpdated)
                 end;
             {error, Reason} ->
                 {reply, {error, Reason}, State}
         end
    end;

handle_call({discard_cards, PlayerPID, CardsFromClient}, _From, State) ->
    if PlayerPID /= State#state.current_player ->
        {reply, {error, not_your_turn}, State};
    true ->
        io:format("~p: [LOGIC] Jugador pide descartar ~w cartas.~n", [PlayerPID, length(CardsFromClient)]),
        PlayerHand = maps:get(PlayerPID, State#state.player_hands),
        {CardsToDiscard, NewHand} = game_model:take_exact_cards(CardsFromClient, PlayerHand, []),
        NewDiscardPile = CardsToDiscard ++ State#state.discard_pile,
        NumToDraw = length(CardsToDiscard),
        {NewDeck, DrawnCards, FinalDiscardPile} = 
            game_model:draw_cards(NumToDraw, State#state.deck, NewDiscardPile),
        FinalHand = DrawnCards ++ NewHand,
        NewPlayerHands = maps:put(PlayerPID, FinalHand, State#state.player_hands),
        StateAfterDiscard = State#state{
            player_hands = NewPlayerHands,
            deck = NewDeck,
            discard_pile = FinalDiscardPile
        },
        {StateFinal, NextAction} = logic_end_turn(PlayerPID, StateAfterDiscard),
        handle_end_turn_action(StateFinal, NextAction)
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({start_turn_process, PlayerPID}, State) ->
    {StateAfterStart, NextAction} = logic_start_turn(PlayerPID, State),
    case NextAction of
        {action_phase, _} ->
            NewState = StateAfterStart#state{game_stage = action_phase},
            broadcast_state(NewState),
            {noreply, NewState};
        {end_turn_after_draw, PlayerPID} ->
            io:format(" -> Jugador ~w terminó turno después de robar 3 cartas.~n", [PlayerPID]),
            PlayerPIDs = StateAfterStart#state.players,
            NextPlayerPID = game_model:next_player(PlayerPID, PlayerPIDs),
            NewState = StateAfterStart#state{
                current_player = NextPlayerPID,
                game_stage = action_phase
            },
            broadcast_state(NewState),
            gen_server:cast(self(), {start_turn_process, NextPlayerPID}),
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast_state(State) ->
    io:format("~p: [LOGIC] broadcast_state llamado. Transmitiendo a ~w jugadores.~n", [self(), length(State#state.players)]),
    StateMap = convert_state_to_map(State),
    Payload = #{
        action => <<"update_state">>,
        state => StateMap
    },
    lists:foreach(
        fun(PlayerPID) ->
            PlayerPID ! {send_json_payload, Payload}
        end,
        State#state.players
    ).

convert_state_to_map(#state{ players = Players,
                             player_hands = PlayerHands,
                             player_boards = PlayerBoards,
                             current_player = CurrentPlayer,
                             game_stage = GameStage,
                             discard_pile = DiscardPile,
                             nicknames = Nicknames 
                           }) ->
    PlayerPIDs_Bin = [list_to_binary(pid_to_list(P)) || P <- Players],
    CurrentPlayer_Bin = list_to_binary(pid_to_list(CurrentPlayer)),
    
    NicknamesMap = maps:fold(fun(P, Name, Acc) -> 
        PidStr = list_to_binary(pid_to_list(P)),
        Acc#{PidStr => Name}
    end, #{}, Nicknames),

    HandsMap = maps:fold(
        fun(PID, Hand, Acc) ->
            PIDStr = list_to_binary(pid_to_list(PID)),
            CardMaps = [#{type => C#card.type, color => C#card.color, name => C#card.name} || C <- Hand],
            Acc#{ PIDStr => CardMaps }
        end,
        #{}, PlayerHands
    ),
    BoardsMap = maps:fold(
        fun(PID, Board, Acc) ->
            PIDStr = list_to_binary(pid_to_list(PID)),
            BoardMap = maps:map(
                fun(_Color, Slot) ->
                    CardMaps = [#{type => C#card.type, color => C#card.color, name => C#card.name} || C <- Slot#organ_slot.cards],
                    #{state => Slot#organ_slot.state, cards => CardMaps}
                end,
                Board
            ),
            Acc#{ PIDStr => BoardMap }
        end,
        #{}, PlayerBoards
    ),
    TopDiscardCard = 
        case DiscardPile of
            [TopCard | _] -> 
                #{type => TopCard#card.type, color => TopCard#card.color, name => TopCard#card.name};
            [] ->
                'null' 
        end,
    
    {StageNameBin, WinnerBin} = case GameStage of
        {game_over, WinnerPID} ->
            WBin = list_to_binary(pid_to_list(WinnerPID)),
            {<<"game_over">>, WBin};
        Atom when is_atom(Atom) ->
            {atom_to_binary(Atom, utf8), null}
    end,

    #{
        players => PlayerPIDs_Bin,
        nicknames => NicknamesMap,
        current_player => CurrentPlayer_Bin,
        game_stage => StageNameBin,
        winner => WinnerBin,
        hands => HandsMap,
        boards => BoardsMap,
        top_discard_card => TopDiscardCard
    }.


handle_contagion_start(PlayerPID, Card, State) ->
    PlayerHand = maps:get(PlayerPID, State#state.player_hands),
    NewHand = lists:delete(Card, PlayerHand),
    NewDiscard = [Card | State#state.discard_pile],
    StateCardPlayed = State#state{
        player_hands = maps:put(PlayerPID, NewHand, State#state.player_hands),
        discard_pile = NewDiscard
    },
    case game_model:can_contagion_happen(PlayerPID, StateCardPlayed#state.player_boards) of
        true ->
            io:format(" -> [CONTAGIO] Iniciando fase interactiva para ~p.~n", [PlayerPID]),
            NewState = StateCardPlayed#state{game_stage = contagion_phase},
            broadcast_state(NewState),
            {reply, {ok, contagion_start}, NewState};
        false ->
            io:format(" -> [CONTAGIO] No hay movimientos posibles. Se pierde la carta.~n"),
            finish_turn_flow(PlayerPID, StateCardPlayed)
    end.

process_normal_play(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State) ->
    {StateAfterMove, Reply} = apply_move(PlayerPID, TargetPID, Card, PlayerColor, TargetColor, State), 
    case Reply of
        {ok, _Played, CardsToDiscardFromLogic} ->
            PlayerHand = maps:get(PlayerPID, StateAfterMove#state.player_hands),
            NewHand_NoPlayed = lists:delete(Card, PlayerHand), 
            AllCardsToDiscard = CardsToDiscardFromLogic, 
            NewDiscardPile_Temp = AllCardsToDiscard ++ StateAfterMove#state.discard_pile,
            Deck = StateAfterMove#state.deck,
            {FinalDeck, [DrawnCard], FinalDiscardPile} = 
                game_model:draw_cards(1, Deck, NewDiscardPile_Temp),
            NewHand = [DrawnCard | NewHand_NoPlayed],
            NewPlayerHands = maps:put(PlayerPID, NewHand, StateAfterMove#state.player_hands),
            StateAfterDraw = StateAfterMove#state{
                player_hands = NewPlayerHands,
                deck = FinalDeck,
                discard_pile = FinalDiscardPile
            },
            {StateFinal, NextAction} = logic_end_turn(PlayerPID, StateAfterDraw),
            handle_end_turn_action(StateFinal, NextAction);
        {error, Reason} ->
            {reply, {error, Reason}, StateAfterMove}
    end.

finish_turn_flow(PlayerPID, State) ->
    CurrentHand = maps:get(PlayerPID, State#state.player_hands),
    CardsNeeded = 3 - length(CurrentHand),
    {NewDeck, DrawnCards, NewDiscard} = game_model:draw_cards(CardsNeeded, State#state.deck, State#state.discard_pile),
    FinalHand = DrawnCards ++ CurrentHand,
    StateRefilled = State#state{
        deck = NewDeck,
        discard_pile = NewDiscard,
        player_hands = maps:put(PlayerPID, FinalHand, State#state.player_hands)
    },
    {StateFinal, NextAction} = logic_end_turn(PlayerPID, StateRefilled),
    handle_end_turn_action(StateFinal, NextAction).

handle_end_turn_action(State, {next_turn, NextPlayerPID}) ->
    NewState = State#state{
        current_player = NextPlayerPID,
        game_stage = action_phase
    },
    broadcast_state(NewState),
    gen_server:cast(self(), {start_turn_process, NextPlayerPID}),
    {reply, {ok, turn_finished}, NewState};

handle_end_turn_action(State, {game_over, WinnerPID}) ->
    NewState = State#state{game_stage = {game_over, WinnerPID}},
    broadcast_state(NewState),
    {reply, {ok, game_over}, NewState}.