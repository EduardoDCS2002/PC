-module(modifier).
-export([new_modifier/0, maybe_spawn_modifier/1, update_modifiers/1, remove_modifier/2]).

-define(SPAWN_PROBABILITY, 0.05).  % 5% spawn chance per tick
-define(MAX_PER_TYPE, 3).
-define(MODIFIER_RADIUS, 50.0).
-define(SCREEN_WIDTH, 1300).       % screen bounds
-define(SCREEN_HEIGHT, 700).

%%% Hardcoded modifier types (as in original)
modifier_types() -> [green, orange, blue, red].

%%% Escolhe aleatoriamente um tipo (corrigido para usar length/1)
random_modifier_type() ->
    Types = modifier_types(),
    lists:nth(rand:uniform(length(Types)), Types).

%%% Cria um novo modificador com tipo, posição, raio e cor
new_modifier() ->
    Type = random_modifier_type(),
    Position = {
        float(rand:uniform(?SCREEN_WIDTH)), 
        float(rand:uniform(?SCREEN_HEIGHT))
    },
    Color = case Type of
        green  -> {0, 255, 0};
        orange -> {255, 165, 0};
        blue   -> {0, 0, 255};
        red    -> {255, 0, 0}
    end,
    {Position, ?MODIFIER_RADIUS, Type, Color}.

%%% Remove um modificador por posição
remove_modifier(Position, Modifiers) ->
    lists:filter(fun({Pos, _, _, _}) -> Pos /= Position end, Modifiers).

%%% Conta modificadores por tipo
count_by_type(Modifiers) ->
    lists:foldl(fun({_, _, Type, _}, Acc) ->
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Modifiers).

%%% Decide se spawna um novo modificador
maybe_spawn_modifier(CurrentModifiers) ->
    case rand:uniform() < ?SPAWN_PROBABILITY of
        true ->
            TypesCount = count_by_type(CurrentModifiers),
            AvailableTypes = [
                T || T <- modifier_types(), 
                maps:get(T, TypesCount, 0) < ?MAX_PER_TYPE
            ],
            case AvailableTypes of
                [] -> CurrentModifiers;
                _  -> [new_modifier() | CurrentModifiers]
            end;
        false ->
            CurrentModifiers
    end.

%%% Atualiza a lista de modificadores (apenas spawn por enquanto)
update_modifiers(Modifiers) ->
    maybe_spawn_modifier(Modifiers).
