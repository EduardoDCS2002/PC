-module(player).
-export([newPlayer/1, update_player_position/2]).
-define(MAX_SPEED, 5.0).
-define(ACCELERATION, 0.2).

%%% Cria um novo player com posição aleatória e velocidade zero (alterar para ser posiçoes opostas ) ??
newPlayer(Id) ->
    Color = 50 + rand:uniform(205),
    Velocity = {0.0, 0.0},
    Position = case Id of
        1 -> {float(rand:uniform(200)), float(rand:uniform(700))};         % Lado esquerdo
        2 -> {float(1100 + rand:uniform(200)), float(rand:uniform(700))}   % Lado direito
    end,
    Score = 0,
    BulletSpeed = 8.0,
    BulletReload = 4.0,
    {Position, Velocity, Color, Score, BulletSpeed, BulletReload}.


%%% Traduz teclas em aceleração acumulada (é possivel que tenha de ser alterado para receber uma lista de teclas)
movement_to_acceleration(Key) ->
    case Key of
        <<"U\n">> -> {0.0, -?ACCELERATION};
        <<"D\n">> -> {0.0, ?ACCELERATION};
        <<"L\n">> -> {-?ACCELERATION, 0.0};
        <<"R\n">> -> {?ACCELERATION, 0.0};
        _ -> {0.0, 0.0}
    end.
%%% Limita a velocidade máxima
clamp(V, Max) when V > Max -> Max;
clamp(V, Max) when V < -Max -> -Max;
clamp(V, _) -> V.

%%% Atualiza a posição do player com base nas teclas 
update_player_position({{Pos, Vel, Color, Score, BS, BR}, UserData}, Key) ->

    {Ax,Ay}= movement_to_acceleration(Key),
    {Vx, Vy} = Vel,
    NewVx = clamp(Vx + Ax, ?MAX_SPEED),
    NewVy = clamp(Vy + Ay, ?MAX_SPEED),
    {X, Y} = Pos,
    NewX = X + NewVx,
    NewY = Y + NewVy,
    
    {{{NewX, NewY}, {NewVx, NewVy}, Color, Score, BS, BR}, UserData}.

%%% Atualiza os buffs do player
update_player_buff({{Pos, Vel, Color, Score, BS, BR}, UserData}, Key) ->

    {Ax,Ay}= movement_to_acceleration(Key),
    {Vx, Vy} = Vel,
    NewVx = clamp(Vx + Ax, ?MAX_SPEED),
    NewVy = clamp(Vy + Ay, ?MAX_SPEED),
    {X, Y} = Pos,
    NewX = X + NewVx,
    NewY = Y + NewVy,
    
    {{{NewX, NewY}, {NewVx, NewVy}, Color, Score, BS, BR}, UserData}.

%%% Atualiza o score da partida
update_player_score({{Pos, Vel, Color, Score, BS, BR}, UserData}, Key) ->

    {Ax,Ay}= movement_to_acceleration(Key),
    {Vx, Vy} = Vel,
    NewVx = clamp(Vx + Ax, ?MAX_SPEED),
    NewVy = clamp(Vy + Ay, ?MAX_SPEED),
    {X, Y} = Pos,
    NewX = X + NewVx,
    NewY = Y + NewVy,
    
    {{{NewX, NewY}, {NewVx, NewVy}, Color, Score, BS, BR}, UserData}.