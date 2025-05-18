-module(collision).
-export([check_collisions_modifiers/2, check_collisions_bullet/2, distance/2, 
         collision_modifier/2, collision_bullet/2,
         check_colision_boards_players/1, check_colision_boards_bullets/1,
         borda/1]).

-define(PLAYER_RADIUS, 50.0).
-define(SCREEN_WIDTH, 1300).
-define(SCREEN_HEIGHT, 700).

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

borda({{_,{X,Y}, _, _, _, _, _, _},{_,_}} ) ->
    X =< -650 + 1300/2 orelse X >= 650 + 1300/2  orelse Y =< -350 + 700/2 orelse Y >= 350 + 700/2.

borda_bullet({{X,Y}, _, _, _} ) ->
    X =< -650 + 1300/2 orelse X >= 650 + 1300/2  orelse Y =< -350 + 700/2 orelse Y >= 350 + 700/2.

collision_modifier({{_,PosPlay, _, _, _, _, _, _}, _}, {PosMod, RadiusMod, _, _}) ->
    distance(PosPlay, PosMod) =< (?PLAYER_RADIUS + RadiusMod - 10.0).

collision_bullet({{_,PosPlay, _, _, _, _, _, _}, _}, {PosBul, _, RadiusBul, _}) ->
    distance(PosPlay, PosBul) =< (?PLAYER_RADIUS + RadiusBul -10.0).

check_collisions_modifiers(Players, Modifiers) ->
    [{Player, Modifier} || 
        Player <- Players, 
        Modifier <- Modifiers, 
        collision_modifier(Player, Modifier)].

check_collisions_bullet(Players, Projectiles) ->
    [{Player, Projectile} || 
        Player <- Players, 
        Projectile <- Projectiles, 
        collision_bullet(Player, Projectile)].

check_colision_boards_players(Players) ->
    [Player || Player <- Players, borda(Player)].

check_colision_boards_bullets(Bullets) ->
    [Bullet || Bullet <- Bullets, borda_bullet(Bullet)].