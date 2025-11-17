-module(game_model).
-export([
    create_and_shuffle_deck/0,
    shuffle_deck/1,
    create_full_deck/0,
    print_deck/1,
    apply_card_to_board/3, % Acepta Card, TargetColor, CurrentBoard (map of #organ_slot{})
    draw_cards/3,
    check_win_condition/1,
    next_player/2,
    remove_card_by_type/2,
    apply_virus/2
]).

-include("virus_defs.hrl").

%% ----------------------------------------------------
%% Creación y Barajado del Mazo
%% ----------------------------------------------------

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

create_card_list([]) ->
    [];
create_card_list([#card{quantity=N}=Card | Rest]) ->
    ActualCard = Card#card{quantity=undefined},
    List = [ActualCard || _ <- lists:seq(1, N)],
    List ++ create_card_list(Rest).

create_full_deck() ->
    AllCardDefinitions = 
        create_organ_deck() ++ 
        create_virus_deck() ++ 
        create_medicine_deck() ++ 
        create_treatment_deck(),

    create_card_list(AllCardDefinitions).

%% Lógica de Barajado (TU CÓDIGO)
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


%% @doc Roba N cartas del mazo, rebarajando el descarte si es necesario.
%% Retorna: {FinalDeck, DrawnCards, NewDiscardPile}
draw_cards(N, Deck, DiscardPile) ->
    case N of
        0 -> 
            {Deck, [], DiscardPile};
        _ ->
            case Deck of
                [] ->
                    % Mazo vacío: Rebarajar el descarte
                    io:format("  -> [MODEL] Mazo vacío. Rebarajando ~w cartas del descarte para robar ~w.~n", 
                              [length(DiscardPile), N]),
                    ShuffledDeck = shuffle_deck(DiscardPile),
                    % Robar de forma recursiva del mazo recién barajado
                    draw_cards(N, ShuffledDeck, []);
                
                [Card | RemainingDeck] ->
                    % Robar la primera carta y seguir robando N-1
                    {FinalDeck, DrawnCardsTail, FinalDiscardPile} = 
                        draw_cards(N - 1, RemainingDeck, DiscardPile),
                    
                    % La carta robada actual va al frente de la cola de cartas robadas
                    {[Card | FinalDeck], [Card | DrawnCardsTail], FinalDiscardPile}
            end
    end.


%% ----------------------------------------------------
%% LÓGICA DE ESTADO DE ÓRGANOS (Trabajando con #organ_slot{})
%% ----------------------------------------------------

is_valid_target(Card, TargetColor, CurrentBoard) ->
    is_valid_target_color(Card, TargetColor) andalso
    is_valid_target_state(Card, TargetColor, CurrentBoard).

%% @doc Verifica si la carta puede ser aplicada al color objetivo (TargetColor es un átomo, ej: red).
is_valid_target_color(Card, TargetColor) ->
    CardColor = Card#card.color,
    (CardColor == TargetColor) orelse (CardColor == ?WILD) orelse (CardColor == ?NONE).

%% @doc Verifica si la carta puede ser aplicada al órgano en su estado actual (excluyendo color).
is_valid_target_state(Card, TargetColor, CurrentBoard) ->
    % IMPORTANTE: CurrentBoard ahora mapea a #organ_slot{}
    CurrentSlot = maps:get(TargetColor, CurrentBoard), 
    CurrentState = CurrentSlot#organ_slot.state, % Extraemos el estado numérico
    CardType = Card#card.type,
    
    case CardType of
        ?T_ORGAN ->
            % Un órgano solo puede jugarse si el espacio está vacío (0)
            CurrentState == 0;
            
        ?T_MEDICINE ->
            % Una medicina no puede jugarse si el órgano está Inmune (3) o Vacío (0).
            % En el juego real, la medicina sobre 0 o 1 (sano) añade inmunidad, así que solo prohibimos 3.
            CurrentState /= 3;
            
        ?T_VIRUS ->
            % Un virus no puede jugarse en un órgano Inmune (3).
            CurrentState /= 3;
            
        ?T_TREATMENT ->
            % Las cartas de Tratamiento (como Contagio, Ladrón) siempre son válidas por estado.
            true;
            
        _ ->
            false
    end.


%% @doc Lógica central: calcula el nuevo estado del tablero después de jugar una carta.
%% Retorna: {ok, TargetColor, NewBoard, CardsToDiscard} | {error, Reason}
apply_card_to_board(Card, TargetColor, CurrentBoard) ->
    
    % 1. VALIDACIÓN COMPLETA
    case is_valid_target(Card, TargetColor, CurrentBoard) of
        false ->
            io:format("    -> [MODEL] ERROR: Jugada inválida por color o estado (~w sobre ~p).~n", 
                      [Card#card.type, TargetColor]),
            {error, invalid_play}; 
        true ->
            % Obtener el slot completo (registro #organ_slot{})
            CurrentSlot = maps:get(TargetColor, CurrentBoard), 
            
            % 2. Aplicar carta y obtener el nuevo slot y las cartas eliminadas del slot
            % Las funciones auxiliares ahora pasan el Slot completo
            {NewSlot, CardsRemovedFromSlot} = 
                case Card#card.type of
                    ?T_ORGAN    -> apply_organ(Card, CurrentSlot);
                    ?T_MEDICINE -> apply_medicine(Card, CurrentSlot);
                    ?T_VIRUS    -> apply_virus(Card, CurrentSlot);
                    ?T_TREATMENT -> {CurrentSlot, []}; % <-- Retorna una lista de descarte vacía
                    _ -> {CurrentSlot, []}
                end,

            io:format("     -> [MODEL] Órgano ~w: Estado ~w -> ~w. Descarte: ~w cartas.~n", 
            [TargetColor, CurrentSlot#organ_slot.state, NewSlot#organ_slot.state, length(CardsRemovedFromSlot)]),
            % 4. Actualizar el tablero
            NewBoard = maps:put(TargetColor, NewSlot, CurrentBoard),
            
            % 5. Retornar el nuevo contrato para el servidor
            {ok, TargetColor, NewBoard, CardsRemovedFromSlot} 
    end.

%% ----------------------------------------------------
%% Lógica Auxiliar de Aplicación de Cartas (Trabaja con #organ_slot{})
%% ----------------------------------------------------

%% @doc Lógica para jugar un Órgano (Solo si el espacio está vacío).
%% Retorna: {NewSlot, CardsRemovedFromSlot}
apply_organ(Card, Slot) ->
    case Slot#organ_slot.state of
        0 -> 
            % 0 (Vacío) -> 1 (Sano). Añade la carta jugada al slot.
            NewCards = [Card | Slot#organ_slot.cards],
            NewSlot = Slot#organ_slot{state = 1, cards = NewCards},
            {NewSlot, []};
        _ -> 
            % Jugada inválida (aunque la validación lo habría prevenido)
            {Slot, []}
    end.

%% @doc Lógica para jugar una Medicina (Cura virus o inmuniza).
%% Retorna: {NewSlot, CardsRemovedFromSlot}
apply_medicine(Card, Slot) ->
    Cards = Slot#organ_slot.cards,
    
    case Slot#organ_slot.state of
        -1 -> 
            {VirusToRemove, RemainingCards} = remove_card_by_type(?T_VIRUS, Cards),
            CardsToRemove = 
                case VirusToRemove of
                    undefined -> [Card];      % Solo la Medicina (Card)
                    V -> [Card, V]          % La Medicina y el Virus removido (V)
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

        _ -> 
            % Otros estados. No debería ocurrir si la validación es correcta.
            {Slot, []}
    end.


%% @doc Lógica para jugar un Virus (Infecta o destruye).
%% Retorna: {NewSlot, CardsRemovedFromSlot}
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
            CardsToRemove = 
                case MedicineToRemove of
                    undefined -> [Card];      
                    M -> [Card, M]          
                end,
            NewSlot = Slot#organ_slot{state = 1, cards = RemainingCards},

            {NewSlot, CardsToRemove};
            
        _ -> 
            {Slot, []}
    end.
    
%% @doc Encuentra y remueve la primera carta de un tipo dado de una lista.
%% Retorna: {CardRemoved, RemainingCards}
remove_card_by_type(_Type, []) ->
    {undefined, []};
remove_card_by_type(Type, [H | T]) ->
    case H#card.type of
        Type -> {H, T}; % Encontrado y removido
        _ -> 
            {Removed, Rest} = remove_card_by_type(Type, T),
            {Removed, [H | Rest]}
    end.

%% @doc Determina el siguiente jugador en la lista.
%% Retorna: PID del siguiente jugador.
next_player(CurrentPlayerPID, PlayerPIDs) ->
    % Rota la lista hasta que el jugador actual esté al frente
    {Front, [CurrentPlayerPID | Rest]} = lists:splitwith(
        fun(PID) -> PID =/= CurrentPlayerPID end, 
        PlayerPIDs
    ),
    
    % El siguiente jugador es el que está en 'Rest', 
    % o el primero de la lista ('Front') si 'Rest' está vacío.
    case Rest of
        [NextPID | _] -> NextPID;
        [] -> hd(Front)
    end.


%% @doc Verifica si algún jugador ha ganado.
%% PlayerBoards es: #{ PID => #{Color => #organ_slot{}} }
%% Retorna: {won, WinnerPID} | no_winner
check_win_condition(PlayerBoards) when is_map(PlayerBoards) ->
    check_win_condition(maps:to_list(PlayerBoards));

check_win_condition([]) ->
    no_winner;
check_win_condition([ {PID, Board} | Rest ]) ->
    % Contar cuántos órganos están sanos (1), protegidos (2) o inmunes (3)
    HealthyOrgans = maps:fold(
        fun(_Color, Slot, Acc) ->
            case Slot#organ_slot.state of
                1 -> Acc + 1;
                2 -> Acc + 1;
                3 -> Acc + 1;
                _ -> Acc
            end
        end,
        0, Board),
    
    % La condición de victoria es 4 órganos sanos/protegidos/inmunes.
    % (Algunas reglas dicen 5 si tienes el órgano multicolor, 
    % pero esta es la implementación más simple)
    if
        HealthyOrgans >= 4 -> {won, PID};
        true -> check_win_condition(Rest)
    end.
