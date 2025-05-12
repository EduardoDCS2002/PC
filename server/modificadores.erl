-module(modifier).
-export([new_modifier/0, maybe_spawn_modifier/1, update_modifiers/1]).

-define(SPAWN_PROBABILITY, 0.05).  % 5% chance de aparecer a cada tick
-define(MAX_PER_TYPE, 3).
-define(MODIFIER_RADIUS, 5).

%%% Tipos de modificadores
modifier_types() -> [green, orange, blue, red].

%%% Cria um novo modificador com tipo, posição, raio e cor
new_modifier() ->
    Type = random_modifier_type(),
    Position = {float(rand:uniform(1300)), float(rand:uniform(700))},
    Color = case Type of
        green -> {0, 255, 0};
        orange -> {255, 165, 0};
        blue -> {0, 0, 255};
        red -> {255, 0, 0}
    end,
    {Position, ?MODIFIER_RADIUS, Type, Color}.

%%% Decide aleatoriamente se deve spawnar novo modificador
maybe_spawn_modifier(CurrentModifiers) ->
    case rand:uniform() < ?SPAWN_PROBABILITY of
        true ->
            TypesCount = count_by_type(CurrentModifiers),
            AvailableTypes = [T || T <- modifier_types(), maps:get(T, TypesCount, 0) < ?MAX_PER_TYPE],
            case AvailableTypes of
                [] -> CurrentModifiers;
                _  -> [new_modifier() | CurrentModifiers]
            end;
        false ->
            CurrentModifiers
    end.

%%% Atualiza a lista de modificadores (neste caso só decide se spawna mais)
update_modifiers(Modifiers) ->
    maybe_spawn_modifier(Modifiers).

%%% Conta quantos modificadores existem por tipo
count_by_type(Modifiers) ->
    lists:foldl(fun({_, _, Type, _}, Acc) ->
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Modifiers).

%%% Escolhe aleatoriamente um tipo
random_modifier_type() ->
    lists:nth(rand:uniform(4), modifier_types()).