-module(game_model).
-export([
    create_and_shuffle_deck/0,
    shuffle_deck/1,
    create_full_deck/0,
    print_deck/1,
    apply_card_to_board/3,
    draw_cards/3,
    check_win_condition/1,
    next_player/2,
    remove_card_by_type/2,
    apply_virus/2,
    take_exact_cards/3,
    can_contagion_happen/2,
    move_virus_card/5
]).

-include("virus_defs.hrl").


create_organ_deck() ->
    [
        #card{type=?T_ORGAN, color=?RED,    name=?N_HEART,      quantity=5},
        #card{type=?T_ORGAN, color=?GREEN,  name=?N_STOMACH,    quantity=5},
        #card{type=?T_ORGAN, color=?BLUE,   name=?N_BRAIN,      quantity=5},
        #card{type=?T_ORGAN, color=?YELLOW, name=?N_BONE,       quantity=5},
        #card{type=?T_ORGAN, color=?WILD,   name=?N_WILD,       quantity=1}
    ].

create_virus_deck() ->
    [
        #card{type=?T_VIRUS, color=?RED,    name=?N_HEART_VIRUS,    quantity=4},
        #card{type=?T_VIRUS, color=?GREEN,  name=?N_STOMACH_VIRUS,  quantity=4},
        #card{type=?T_VIRUS, color=?BLUE,   name=?N_BRAIN_VIRUS,    quantity=4},
        #card{type=?T_VIRUS, color=?YELLOW, name=?N_BONE_VIRUS,     quantity=4},
        #card{type=?T_VIRUS, color=?WILD,   name=?N_WILD_VIRUS,     quantity=1}
    ].

create_medicine_deck() ->
    [
        #card{type=?T_MEDICINE, color=?RED,    name=?N_HEART_MEDICINE,       quantity=4},
        #card{type=?T_MEDICINE, color=?GREEN,  name=?N_STOMACH_MEDICINE,     quantity=4},
        #card{type=?T_MEDICINE, color=?BLUE,   name=?N_BRAIN_MEDICINE,       quantity=4},
        #card{type=?T_MEDICINE, color=?YELLOW, name=?N_BONE_MEDICINE,        quantity=4},
        #card{type=?T_MEDICINE, color=?WILD,   name=?N_WILD_MEDICINE,        quantity=4}
    ].

create_treatment_deck() ->
    [
        #card{type=?T_TREATMENT, color=?NONE, name=?N_CONTAGION,      quantity=2},
        #card{type=?T_TREATMENT, color=?NONE, name=?N_ORGAN_THIEF,    quantity=3},
        #card{type=?T_TREATMENT, color=?NONE, name=?N_TRANSPLANT,     quantity=3},
        #card{type=?T_TREATMENT, color=?NONE, name=?N_LATEX_GLOVE,    quantity=1},
        #card{type=?T_TREATMENT, color=?NONE, name=?N_MEDICAL_MISTAKE, quantity=1}
    ].

create_card_list([]) -> [];
create_card_list([#card{quantity=N}=Card | Rest]) ->
    ActualCard = Card#card{quantity=undefined},
    List = [ActualCard || _ <- lists:seq(1, N)],
    List ++ create_card_list(Rest).

create_full_deck() ->
    AllCardDefinitions = create_organ_deck() ++ create_virus_deck() ++ create_medicine_deck() ++ create_treatment_deck(),
    create_card_list(AllCardDefinitions).

create_and_shuffle_deck() ->
    FullDeck = create_full_deck(),
    Seed = {erlang:system_time(), erlang:unique_integer([monotonic]), erlang:phash2(self())},
    rand:seed(exs1024, Seed),
    shuffle(FullDeck).

shuffle_deck(Deck) -> shuffle(Deck).
shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    Len = length(List),
    I = rand:uniform(Len),
    {Elem, Rest} = remove_at(I, List),
    shuffle(Rest, [Elem | Acc]).

remove_at(1, [H|T]) -> {H, T};
remove_at(N, [H|T]) when N > 1 ->
    {Elem, Rest} = remove_at(N-1, T),
    {Elem, [H | Rest]}.

print_deck(Deck) ->
    io:format("--- Mazo Completo (~w cartas) ---~n", [length(Deck)]),
    print_deck(Deck, 1),
    io:format("---------------------------------~n", []).

print_deck([], _Counter) -> ok;
print_deck([Card | Rest], Counter) ->
    io:format("~2w: ~p~n", [Counter, Card]),
    print_deck(Rest, Counter + 1).

draw_cards(N, Deck, DiscardPile) ->
    case N of
        0 -> {Deck, [], DiscardPile};
        _ ->
            case Deck of
                [] ->
                    io:format("  -> [MODEL] Mazo vacío. Rebarajando ~w cartas del descarte para robar ~w.~n", [length(DiscardPile), N]),
                    ShuffledDeck = shuffle_deck(DiscardPile),
                    draw_cards(N, ShuffledDeck, []);
                [Card | RemainingDeck] ->
                    {FinalDeck, DrawnCardsTail, FinalDiscardPile} = draw_cards(N - 1, RemainingDeck, DiscardPile),
                    {FinalDeck, [Card | DrawnCardsTail], FinalDiscardPile}
            end
    end.

is_valid_target(Card, TargetColor, CurrentBoard) ->
    is_valid_target_color(Card, TargetColor) andalso is_valid_target_state(Card, TargetColor, CurrentBoard).

is_valid_target_color(Card, TargetColor) ->
    CardColor = Card#card.color,
    case Card#card.type of
        ?T_ORGAN -> (CardColor == TargetColor);
        _ -> (CardColor == TargetColor) orelse (CardColor == ?WILD) orelse (TargetColor == ?WILD)
    end.

is_valid_target_state(Card, TargetColor, CurrentBoard) ->
    CurrentSlot = maps:get(TargetColor, CurrentBoard), 
    CurrentState = CurrentSlot#organ_slot.state, 
    CardType = Card#card.type,
    case CardType of
        ?T_ORGAN -> CurrentState == 0;
        ?T_MEDICINE -> CurrentState /= 3 andalso CurrentState /= 0 andalso is_valid_medicine_or_virus(Card, TargetColor, CurrentBoard);
        ?T_VIRUS -> CurrentState /= 3 andalso CurrentState /= 0 andalso is_valid_medicine_or_virus(Card, TargetColor, CurrentBoard);
        ?T_TREATMENT -> true;
        _ -> false
    end.

is_valid_medicine_or_virus(Card, TargetColor, CurrentBoard) ->
    CurrentSlot = maps:get(TargetColor, CurrentBoard), 
    CurrentState = CurrentSlot#organ_slot.state, 
    CurrentCards = CurrentSlot#organ_slot.cards,
    CardType = Card#card.type,
    
    case CardType of
        ?T_MEDICINE when CurrentState == -1 ->
            VirusList = lists:filter(fun(C) -> C#card.type == ?T_VIRUS end, CurrentCards),
            
            case VirusList of
                [VirusCard | _] -> 
                    Card#card.color == VirusCard#card.color orelse 
                    Card#card.color == ?WILD orelse 
                    VirusCard#card.color == ?WILD;
                [] -> 
                    false
            end;

        ?T_VIRUS when CurrentState == 1 -> 
            MedicineList = lists:filter(fun(C) -> C#card.type == ?T_MEDICINE end, CurrentCards),
            
            case MedicineList of
                [MedicineCard | _] ->
                    Card#card.color == MedicineCard#card.color orelse 
                    Card#card.color == ?WILD orelse 
                    MedicineCard#card.color == ?WILD;
                [] ->
                    true 
            end;
            
        _ -> 
            true 
    end.

apply_card_to_board(Card, TargetColor, CurrentBoard) ->
    case is_valid_target(Card, TargetColor, CurrentBoard) of
        false ->
            io:format("    -> [MODEL] ERROR: Jugada inválida por color o estado (~w sobre ~p).~n", [Card#card.type, TargetColor]),
            {error, invalid_play}; 
        true ->
            CurrentSlot = maps:get(TargetColor, CurrentBoard), 
            {NewSlot, CardsRemovedFromSlot} = 
                case Card#card.type of
                    ?T_ORGAN    -> apply_organ(Card, CurrentSlot);
                    ?T_MEDICINE -> apply_medicine(Card, CurrentSlot);
                    ?T_VIRUS    -> apply_virus(Card, CurrentSlot);
                    ?T_TREATMENT -> {CurrentSlot, []};
                    _ -> {CurrentSlot, []}
                end,
            io:format("     -> [MODEL] Órgano ~w: Estado ~w -> ~w. Descarte: ~w cartas.~n", 
            [TargetColor, CurrentSlot#organ_slot.state, NewSlot#organ_slot.state, length(CardsRemovedFromSlot)]),
            NewBoard = maps:put(TargetColor, NewSlot, CurrentBoard),
            {ok, TargetColor, NewBoard, CardsRemovedFromSlot} 
    end.

apply_organ(Card, Slot) ->
    case Slot#organ_slot.state of
        0 -> 
            NewCards = [Card | Slot#organ_slot.cards],
            NewSlot = Slot#organ_slot{state = 1, cards = NewCards},
            {NewSlot, []};
        _ -> {Slot, []}
    end.

apply_medicine(Card, Slot) ->
    Cards = Slot#organ_slot.cards,
    case Slot#organ_slot.state of
        -1 -> 
            {VirusToRemove, RemainingCards} = remove_card_by_type(?T_VIRUS, Cards),
            CardsToRemove = case VirusToRemove of
                    undefined -> [Card];     
                    V -> [Card, V]        
                end,
            NewSlot = Slot#organ_slot{state = 1, cards = RemainingCards},
            {NewSlot, CardsToRemove};
        1 ->
            NewCards = [Card | Cards],
            NewSlot = Slot#organ_slot{state = 2, cards = NewCards},
            {NewSlot, []};
        2 ->
            NewCards = [Card | Cards],
            NewSlot = Slot#organ_slot{state = 3, cards = NewCards},
            {NewSlot, []};
        _ -> {Slot, []}
    end.

apply_virus(Card, Slot) -> 
    Cards = Slot#organ_slot.cards,
    case Slot#organ_slot.state of
        1 -> 
            NewCards = [Card | Cards],
            NewSlot = Slot#organ_slot{state = -1, cards = NewCards},
            {NewSlot, []};
        -1 -> 
            NewSlot = Slot#organ_slot{state = 0, cards = []},
            {NewSlot, [Card | Cards]}; 
        2 -> 
            {MedicineToRemove, RemainingCards} = remove_card_by_type(?T_MEDICINE, Cards),
            CardsToRemove = case MedicineToRemove of
                    undefined -> [Card];      
                    M -> [Card, M]          
                end,
            NewSlot = Slot#organ_slot{state = 1, cards = RemainingCards},
            {NewSlot, CardsToRemove};
        _ -> {Slot, []}
    end.
    
remove_card_by_type(_Type, []) -> {undefined, []};
remove_card_by_type(Type, [H | T]) ->
    case H#card.type of
        Type -> {H, T};
        _ -> 
            {Removed, Rest} = remove_card_by_type(Type, T),
            {Removed, [H | Rest]}
    end.

next_player(CurrentPlayerPID, PlayerPIDs) ->
    {Front, [CurrentPlayerPID | Rest]} = lists:splitwith(
        fun(PID) -> PID =/= CurrentPlayerPID end, PlayerPIDs),
    case Rest of
        [NextPID | _] -> NextPID;
        [] -> hd(Front)
    end.

check_win_condition(PlayerBoards) when is_map(PlayerBoards) -> check_win_condition(maps:to_list(PlayerBoards));
check_win_condition([]) -> no_winner;
check_win_condition([ {PID, Board} | Rest ]) ->
    HealthyOrgans = maps:fold(
        fun(_Color, Slot, Acc) ->
            case Slot#organ_slot.state of
                1 -> Acc + 1; 2 -> Acc + 1; 3 -> Acc + 1; _ -> Acc
            end
        end, 0, Board),
    if HealthyOrgans >= 4 -> {won, PID}; true -> check_win_condition(Rest) end.

take_exact_cards([], PlayerHand, Acc) -> {Acc, PlayerHand};
take_exact_cards([CardToDiscard | RestCards], PlayerHand, Acc) ->
    case lists:member(CardToDiscard, PlayerHand) of
        true ->
            NewPlayerHand = lists:delete(CardToDiscard, PlayerHand),
            take_exact_cards(RestCards, NewPlayerHand, [CardToDiscard | Acc]);
        false ->
            take_exact_cards(RestCards, PlayerHand, Acc)
    end.

can_contagion_happen(PlayerPID, PlayerBoards) ->
    MyBoard = maps:get(PlayerPID, PlayerBoards),
    
    maps:fold(fun(SourceColor, MySlot, AnyMoveFound) ->
        if AnyMoveFound -> true;
           MySlot#organ_slot.state == -1 -> 
               {VirusCard, _} = remove_card_by_type(?T_VIRUS, MySlot#organ_slot.cards),
               IsWildVirus = (VirusCard#card.color == ?WILD),
               check_opponents_for_target(PlayerPID, PlayerBoards, SourceColor, IsWildVirus);
           true -> false
        end
    end, false, MyBoard).

check_opponents_for_target(PlayerPID, PlayerBoards, SourceColor, IsWildVirus) ->
    maps:fold(fun(OpponentPID, OppBoard, AccFound) ->
        if AccFound -> true;
           OpponentPID == PlayerPID -> false;
           true ->
                TargetColorsToCheck = case IsWildVirus of
                    true -> maps:keys(OppBoard);
                    false -> [SourceColor]
                end,
                lists:any(fun(TColor) ->
                    Slot = maps:get(TColor, OppBoard, #organ_slot{}),
                    Slot#organ_slot.state == 1
                end, TargetColorsToCheck)
        end
    end, false, PlayerBoards).

move_virus_card(PlayerPID, TargetPID, SourceColor, TargetColor, PlayerBoards) ->
    PlayerBoard = maps:get(PlayerPID, PlayerBoards),
    TargetBoard = maps:get(TargetPID, PlayerBoards),
    
    MySlot = maps:get(SourceColor, PlayerBoard),
    TargetSlot = maps:get(TargetColor, TargetBoard),
    
    StatesOK = (MySlot#organ_slot.state == -1) andalso (TargetSlot#organ_slot.state == 1),
    
    if not StatesOK -> {error, invalid_slot_state};
    true ->
        {VirusCard, RemainingCards} = remove_card_by_type(?T_VIRUS, MySlot#organ_slot.cards),

        ColorsMatch = (VirusCard#card.color == TargetColor),
        IsWildVirus = (VirusCard#card.color == ?WILD),
        IsWildTarget = (TargetColor == ?WILD), 
        
        if (ColorsMatch orelse IsWildVirus orelse IsWildTarget) ->
             NewMySlot = MySlot#organ_slot{state = 1, cards = RemainingCards},
             NewTargetCards = [VirusCard | TargetSlot#organ_slot.cards],
             NewTargetSlot = TargetSlot#organ_slot{state = -1, cards = NewTargetCards},
             
             NewPlayerBoard = maps:put(SourceColor, NewMySlot, PlayerBoard),
             NewTargetBoard = maps:put(TargetColor, NewTargetSlot, TargetBoard),
             
             FinalBoards = maps:put(PlayerPID, NewPlayerBoard, 
                           maps:put(TargetPID, NewTargetBoard, PlayerBoards)),
             {ok, FinalBoards};
        true ->
             {error, color_mismatch}
        end
    end.