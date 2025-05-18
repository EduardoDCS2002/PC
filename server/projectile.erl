-module(projectile).
-export([new_projectile/3, update_projectiles/1, update_projectile/1, filter_expired/1, remove_bullets/2]).

-define(PROJECTILE_RADIUS, 15.0).
-define(PROJECTILE_SPEED, 8.0).  % Pixels per update
-define(PROJECTILE_LIFESPAN_MS, 2000).
-define(SCREEN_WIDTH, 1300).
-define(SCREEN_HEIGHT, 700).


%%%new_projectile({PlayerX, PlayerY}, {PlayerX, PlayerY}) ->  % Handle zero distance
    %%%{PlayerX, PlayerY}, {0.0, 0.0}, ?PROJECTILE_RADIUS, erlang:system_time(millisecond)};
new_projectile({PlayerX, PlayerY}, {CursorX, CursorY}, BulletSpeed) ->
    Dx = CursorX - PlayerX,
    Dy = CursorY - PlayerY,
    Angle = math:atan2(Dy, Dx),

    Offset = 50.0 + 5.0,  % raio do jogador

    % Calcular posição inicial da bala fora do jogador
    SpawnX = PlayerX + Offset * math:cos(Angle),
    SpawnY = PlayerY + Offset * math:sin(Angle),

    Vx =  BulletSpeed * math:cos(Angle),
    Vy =  BulletSpeed * math:sin(Angle),
    T = erlang:system_time(millisecond),
    {{SpawnX, SpawnY}, {Vx, Vy}, ?PROJECTILE_RADIUS, T}.

filter_out_of_bounds(Projectiles) ->
    [P || P = {{X, Y}, _, _, _} <- Projectiles,
           X >= 0, X =< ?SCREEN_WIDTH,
           Y >= 0, Y =< ?SCREEN_HEIGHT].

update_projectile({{X, Y}, {Vx, Vy}, Radius, Timestamp}) ->
    {{X + Vx, Y + Vy}, {Vx, Vy}, Radius, Timestamp}.

update_projectiles(Projectiles) ->
    Projectiles_In_Bounds = filter_out_of_bounds(Projectiles),
    [update_projectile(Proj) || Proj <- Projectiles_In_Bounds].


filter_expired(Projectiles) ->
    Now = erlang:system_time(millisecond),
    [P || P = {_, _, _, T} <- Projectiles, Now - T =< ?PROJECTILE_LIFESPAN_MS].

%%% Remove um modificador por posição
remove_bullets(CollidedPositions, Bullets) ->
    lists:filter(
    fun({Pos, _, _, _}) ->
            not lists:member(Pos, CollidedPositions)
        end,
        Bullets
    ).
