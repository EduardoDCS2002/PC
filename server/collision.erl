-module(collision).
-export([check_collisions_modifiers/2, check_collisions_bullet/2, distance/2, collision_modifier/2, collision_bullet/2,check_colision_boards_players/1,check_colision_boards_bullet/1,borda/1]).

-import(math, [sqrt/1, pow/2]).

distance({X1, Y1}, {X2, Y2}) ->
    sqrt(pow(X2 - X1, 2) + pow(Y2 - Y1, 2)).

check_collisions_modifiers(Players, Modifiers) ->
    [{Player, Modifeir} || Player <- Players, Modifier <- Modifiers, collision_modifer(Player, Modifier)].

check_collisions_bullet(Players, Projectlies) ->
    [{Player, Projectile} || Player <- Players, Projectile <- Projectiles, collision_bullet(Player, Projectile)].  

check_colision_boards_players(Players) ->
    [Player || Player <- Players, borda(Player)].

check_colision_boards_bullet(Projectiles) ->
    [Projectile || Projectile <- Projectiles, borda(Projectile)].

borda({{{X,Y}, _, _},{_,_}} ) ->
    X =< -650 + 1300/2 orelse X >= 650 + 1300/2  orelse Y =< -350 + 700/2 orelse Y >= 350 + 700/2.

collision_modifier({{PosPlay, _, _},{_,_}}, {PosModi, RadiusModi, _, _}) ->
    distance(PosPlay, PosModi) =< (20.0 + RadiusModi).

collision_bullet({{PosPlay, _, _},{_,_}}, {PosBul, _, RadiusBul,_}) ->
    distance(PosPlay, PosBul) =< (20.0 + RadiusBul).
