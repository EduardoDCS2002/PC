-module(players).
-export([newPlayer/0, update_player_position/2]).
-define(MAX_SPEED, 5.0).
-define(ACCELERATION, 0.2).

%%% Cria um novo player com posição aleatória e velocidade zero (alterar para ser posiçoes opostas ) ??
newPlayer() ->
    Color = 50 + rand:uniform(205),
    Velocity = {0.0, 0.0},
    Position = {float(rand:uniform(1300))+50, float(rand:uniform(700))+50},
    {Position, Velocity, Color}.


%%% Traduz teclas em aceleração acumulada (é possivel que tenha de ser alterado para receber uma lista de teclas)
movement_to_acceleration(Key) ->
    case Key of
        <<"U\n">> -> {0.0, -?ACCELERATION};
        <<"D\n">> -> {0.0, ?ACCELERATION};
        <<"L\n">> -> {-?ACCELERATION, 0.0};
        <<"R\n">> -> {?ACCELERATION, 0.0};
        _ -> {0.0, 0.0};
    end.
%%% Limita a velocidade máxima
clamp(V, Max) when V > Max -> Max;
clamp(V, Max) when V < -Max -> -Max;
clamp(V, _) -> V.

%%% Atualiza a posição do player com base nas teclas 
update_player_position({{Pos, Vel, Color}, UserData}, Key) ->

    {Ax,Ay}= movement_to_acceleration(Key),
    {Vx, Vy} = Vel,
    NewVx = clamp(Vx + Ax, ?MAX_SPEED),
    NewVy = clamp(Vy + Ay, ?MAX_SPEED),
    {X, Y} = Pos,
    NewX = X + NewVx,
    NewY = Y + NewVy,
    
    {{{NewX, NewY}, {NewVx, NewVy}, Color}, UserData}.