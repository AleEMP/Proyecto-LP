%%% Archivo: testing.erl

% TEST DE APPLY_CARD_TO_BOARD/3

-module(test_ACTB).
-export([
    test_all/0
]).

-include("virus_defs.hrl").
-import(game_model, [apply_card_to_board/3]).

%% ----------------------------------------------------
%% Estructuras de Soporte para Pruebas
%% ----------------------------------------------------

% Función auxiliar para crear un tablero vacío de 4 colores
initial_board() ->
    lists:foldl(fun(Color, Map) -> 
        maps:put(Color, #organ_slot{}, Map) % Inicializa con #organ_slot{} (state=0, cards=[])
    end,
    #{}, ?ORGAN_COLORS).

% Función auxiliar para crear un slot en un estado específico
make_slot(State, Cards) ->
    #organ_slot{state = State, cards = Cards}.

% Cartas de ejemplo para las pruebas
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
    
%% ----------------------------------------------------
%% Función Principal de Pruebas
%% ----------------------------------------------------

%% @doc Ejecuta todas las pruebas para game_model:apply_card_to_board/3.
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


%% ----------------------------------------------------
%% Casos de Prueba Específicos
%% ----------------------------------------------------

%% Caso 1: Órgano sobre slot vacío (0 -> 1)
test_organ_on_empty() ->
    Board = initial_board(),
    Card = organ_red(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Card] andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 2: Órgano sobre slot ocupado (Inválido)
test_organ_on_occupied_invalid() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = organ_green(), % Carta a jugar (no importa el color)
    
    % Aplicar carta
    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.

%% Caso 3: Virus sobre órgano sano (1 -> -1)
test_virus_on_sane() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = virus_red(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == -1 andalso 
         NewSlot#organ_slot.cards == [Card, Organ] andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 4: Virus sobre órgano infectado (-1 -> 0, Muerte)
test_virus_on_infected() ->
    Organ = organ_red(),
    Virus = virus_red(),
    Board = maps:put(?RED, make_slot(-1, [Virus, Organ]), initial_board()),
    Card = virus_red(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    % La carta jugada (Card) + las cartas en el slot ([Virus, Organ])
    ExpectedDiscard = [Card, Virus, Organ], 
    case NewSlot#organ_slot.state == 0 andalso 
         NewSlot#organ_slot.cards == [] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 5: Virus sobre órgano Inmune (3) (Inválido)
test_virus_on_immune_invalid() ->
    Organ = organ_red(),
    Med1 = med_red_1(),
    Med2 = med_red_2(),
    Board = maps:put(?RED, make_slot(3, [Med2, Med1, Organ]), initial_board()),
    Card = virus_red(),
    
    % Aplicar carta
    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.

%% Caso 6: Virus sobre órgano con 1 Medicina (2 -> 1, Destruye Medicina)
test_virus_on_protected() ->
    Organ = organ_red(),
    Med = med_red_1(),
    Board = maps:put(?RED, make_slot(2, [Med, Organ]), initial_board()),
    Card = virus_red(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    % La carta jugada (Card) + la Medicina removida (Med)
    ExpectedDiscard = [Card, Med], 
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Organ] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 7: Medicina sobre órgano infectado (-1 -> 1, Cura Virus)
test_medicine_on_infected() ->
    Organ = organ_red(),
    Virus = virus_red(),
    Board = maps:put(?RED, make_slot(-1, [Virus, Organ]), initial_board()),
    Card = med_red_1(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    % La carta jugada (Card) + el Virus removido (Virus)
    ExpectedDiscard = [Card, Virus], 
    case NewSlot#organ_slot.state == 1 andalso 
         NewSlot#organ_slot.cards == [Organ] andalso
         lists:sort(Discard) == lists:sort(ExpectedDiscard) of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 8: Medicina sobre órgano sano (1 -> 2)
test_medicine_on_sane() ->
    Organ = organ_red(),
    Board = maps:put(?RED, make_slot(1, [Organ]), initial_board()),
    Card = med_wild(), % Usamos Wild Medicine para probar compatibilidad
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 2 andalso 
         lists:sort(NewSlot#organ_slot.cards) == lists:sort([Card, Organ]) andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.

%% Caso 9: Medicina sobre órgano con 1 medicina (2 -> 3, Inmunidad)
test_medicine_on_single_protected() ->
    Organ = organ_red(),
    Med = med_red_1(),
    Board = maps:put(?RED, make_slot(2, [Med, Organ]), initial_board()),
    Card = med_red_2(),
    
    % Aplicar carta
    {ok, ?RED, NewBoard, Discard} = apply_card_to_board(Card, ?RED, Board),
    
    % Validar estado final
    NewSlot = maps:get(?RED, NewBoard),
    case NewSlot#organ_slot.state == 3 andalso 
         lists:sort(NewSlot#organ_slot.cards) == lists:sort([Card, Med, Organ]) andalso
         Discard == [] of
        true -> ok;
        false -> error({unexpected_result, NewSlot, Discard})
    end.
    
%% Caso 10: Medicina sobre órgano inmunizado (3) (Inválido)
test_medicine_on_immune_invalid() ->
    Organ = organ_red(),
    Med1 = med_red_1(),
    Med2 = med_red_2(),
    Board = maps:put(?RED, make_slot(3, [Med2, Med1, Organ]), initial_board()),
    Card = med_red_1(),
    
    % Aplicar carta
    {error, invalid_play} = apply_card_to_board(Card, ?RED, Board),
    ok.
