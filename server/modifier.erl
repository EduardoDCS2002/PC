-module(modifier).
-export([new_modifier/1, maybe_spawn_modifier/1, update_modifiers/1, remove_modifier/2]).

-define(SPAWN_PROBABILITY, 0.05).  % 5% spawn chance per tick
-define(MAX_PER_TYPE, 3).
-define(MODIFIER_RADIUS, 30.0).
-define(SCREEN_WIDTH, 1300).       % screen bounds
-define(SCREEN_HEIGHT, 700).

%%% Hardcoded modifier types (as in original)
modifier_types() -> [green, orange, blue, red].

random_element(List) ->
    Length = length(List),
    Index = rand:uniform(Length) - 1,  % uniform(1..N) → adjust to 0-based
    lists:nth(Index + 1, List).       % lists:nth is 1-based

%%% Cria um novo modificador com tipo, posição, raio e cor
new_modifier(Type) ->
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
remove_modifier(CollidedPositions, Modifiers) ->
    lists:filter(
    fun({Pos, _, _, _}) ->
            not lists:member(Pos, CollidedPositions)
        end,
        Modifiers
    ).

%%% Conta modificadores por tipo
count_by_type(Modifiers) ->
    lists:foldl(fun({_, _, Type, _}, Acc) ->
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Modifiers).

%%% Decide se spawna um novo modificador
maybe_spawn_modifier(CurrentModifiers) ->
     Chance = rand:uniform(),    
    case Chance < ?SPAWN_PROBABILITY andalso Chance > 0 of
        true ->
            TypesCount = count_by_type(CurrentModifiers),
            AvailableTypes = [
                T || T <- modifier_types(), 
                maps:get(T, TypesCount, 0) < ?MAX_PER_TYPE
            ],
            case AvailableTypes of
                [] -> CurrentModifiers;
                H  -> [new_modifier(random_element(H)) | CurrentModifiers]
            end;
        false ->
            CurrentModifiers
    end.

%%% Atualiza a lista de modificadores (apenas spawn por enquanto)
update_modifiers(Modifiers) ->
    maybe_spawn_modifier(Modifiers).
