-module(projectile).
-export([new_projectile/2, update_projectiles/1, filter_expired/1]).

-import(math, [sqrt/1, pow/2, atan2/2, cos/1, sin/1]).

-define(PROJECTILE_RADIUS, 3).
-define(PROJECTILE_SPEED, 8.0).
-define(PROJECTILE_LIFESPAN, 2000).

%%% Cria um novo projétil com base na posição do jogador e do cursor
%%% Pode ser necessario meter o ID do jogador
new_projectile({PlayerX, PlayerY}, {CursorX, CursorY}) ->
    Dx = CursorX - PlayerX,
    Dy = CursorY - PlayerY,
    Angle = atan2(Dy, Dx),
    Vx = ?PROJECTILE_SPEED * cos(Angle),
    Vy = ?PROJECTILE_SPEED * sin(Angle),
    Velocity = {Vx, Vy},
    Position = {PlayerX, PlayerY},
    Timestamp = erlang:system_time(millisecond),
    {Position, Velocity, ?PROJECTILE_RADIUS, Timestamp}.

%%% Atualiza a posição de todos os projéteis
update_projectiles(Projectiles) ->
    [update_projectile(Proj) || Proj <- Projectiles].

%%% Atualiza um projétil individual
update_projectile({{X, Y}, {Vx, Vy}, Radius, Timestamp}) ->
    NewX = X + Vx,
    NewY = Y + Vy,
    {{NewX, NewY}, {Vx, Vy}, Radius, Timestamp}.

%%% Remove projéteis antigos que já passaram do tempo de vida
filter_expired(Projectiles) ->
    Now = erlang:system_time(millisecond),
    [B || B = {_, _, _, T} <- Projectiles, Now - T =< ?PROJECTILE_LIFESPAN].
