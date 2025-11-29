    
-module(test_ACTB).
-export([
    test_all/0
]).

-include("virus_defs.hrl").
-import(game_model, [apply_card_to_board/3]).

initial_board() ->
    lists:foldl(fun(Color, Map) -> 
        maps:put(Color, #organ_slot{}, Map) 
    end,
    #{}, ?ORGAN_COLORS).

make_slot(State, Cards) ->
    #organ_slot{state = State, cards = Cards}.

organ_red() -> 
    #card{type=?T_ORGAN, color=?RED, name=?N_HEART, quantity=undefined}.
organ_green() -> 
    #card{type=?T_ORGAN, color=?GREEN, name=?N_STOMACH, quantity=undefined}.
virus_red() -> 
    #card{type=?T_VIRUS, color=?RED, name=?N_HEART_VIRUS, quantity=undefined}.
med_red_1() -> 
    #card{type=?T_MEDICINE, color=?RED, name=?N_HEART_MEDICINE, quantity=undefined}.
med_red_2() -> 
    #card{type=?T_MEDICINE, color=?RED, name=?N_HEART_MEDICINE, quantity=undefined}.
med_wild() -> 
    #card{type=?T_MEDICINE, color=?WILD, name=?N_WILD_MEDICINE, quantity=undefined}.
    
test_all() ->
    io:format("--- Iniciando Pruebas de apply_card_to_board/3 ---~n"),
    
    TestCases = [
        % 1. ÓRGANO
        {"Organ: Jugar en slot vacío (0 -> 1)", fun test_organ_on_empty/0},
        {"Organ: Jugar en slot ocupado (Invalido)", fun test_organ_on_occupied_invalid/0},
        
        % 2. VIRUS
        {"Virus: Infectar órgano sano (1 -> -1)", fun test_virus_on_sane/0},
        {"Virus: Sobre órgano infectado (Mata: -1 -> 0)", fun test_virus_on_infected/0},
        {"Virus: Sobre órgano doble-medicina (Inmune: Invalido)", fun test_virus_on_immune_invalid/0},
        {"Virus: Sobre órgano con 1 medicina (Destruye: 2 -> 1)", fun test_virus_on_protected/0},
        
        % 3. MEDICINA
        {"Medicine: Curar órgano infectado (-1 -> 1)", fun test_medicine_on_infected/0},
        {"Medicine: Inmunidad 1 (1 -> 2)", fun test_medicine_on_sane/0},
        {"Medicine: Inmunidad 2 (2 -> 3)", fun test_medicine_on_single_protected/0},
        {"Medicine: Sobre órgano inmunizado (Invalido)", fun test_medicine_on_immune_invalid/0}
    ],

    Results = lists:map(fun({Name, Func}) ->
        try
            Result = Func(),
            io:format("  [PASS] ~s~n", [Name]),
            {Name, pass, Result}
        catch
            error:Reason ->
                io:format("  [FAIL] ~s - Razón: ~p~n", [Name, Reason]),
                {Name, fail, Reason}
        end
    end, TestCases),
    
    Passed = length([ok || {_, pass, _} <- Results]),
    Total = length(Results),
    io:format("--- Resumen: ~w de ~w pruebas pasadas. ---~n", [Passed, Total]),
    
    {Passed, Total}.

test_organ_on_empty() ->
    Board = initial_board(),
    Card = organ_red(),
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Card] andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_organ_on_occupied_invalid() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = organ_green(), 
    
    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.

test_virus_on_sane() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = virus_red(),
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == -1 andalso 
         NewSlot#organ_slot.cards == [Card, Organ] andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_virus_on_infected() ->
    Organ = organ_red(),
    Virus = virus_red(),
    Board = maps:put(?RED, make_slot(-1, [Virus, Organ]), initial_board()),
    Card = virus_red(),
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    ExpectedDiscard = [Card, Virus, Organ], 
    case NewSlot#organ_slot.state == 0 andalso 
         NewSlot#organ_slot.cards == [] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_virus_on_immune_invalid() ->
    Organ = organ_red(),
    Med1 = med_red_1(),
    Med2 = med_red_2(),
    Board = maps:put(?RED, make_slot(3, [Med2, Med1, Organ]), initial_board()),
    Card = virus_red(),
    
    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.

test_virus_on_protected() ->
    Organ = organ_red(),
    Med = med_red_1(),
    Board = maps:put(?RED, make_slot(2, [Med, Organ]), initial_board()),
    Card = virus_red(),
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    ExpectedDiscard = [Card, Med], 
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Organ] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_medicine_on_infected() ->
    Organ = organ_red(),
    Virus = virus_red(),
    Board = maps:put(?RED, make_slot(-1, [Virus, Organ]), initial_board()),
    Card = med_red_1(),
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    ExpectedDiscard = [Card, Virus], 
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Organ] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_medicine_on_sane() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = med_wild(), 
    
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 2 andalso 
         lists:sort(NewSlot#organ_slot.cards) == lists:sort([Card, Organ]) andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

test_medicine_on_single_protected() ->
    Organ = organ_red(),
    Med = med_red_1(),
    Board = maps:put(?RED, make_slot(2, [Med, Organ]), initial_board()),
    Card = med_red_2(),

    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 3 andalso 
         lists:sort(NewSlot#organ_slot.cards) == lists:sort([Card, Med, Organ]) andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.
    
test_medicine_on_immune_invalid() ->
    Organ = organ_red(),
    Med1 = med_red_1(),
    Med2 = med_red_2(),
    Board = maps:put(?RED, make_slot(3, [Med2, Med1, Organ]), initial_board()),
    Card = med_red_1(),

    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.
