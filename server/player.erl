-module(player).
-export([newPlayer/1, update_players_position_reset/1, update_player_position/2, update_player_modifier/2, update_player_score/2, update_player_decay/1, update_players_decay/1]).
-define(MAX_SPEED, 5.0).
-define(ACCELERATION, 0.2).
-define(BULLETCHANGESPEED, 2.0).
-define(BULLETCHANGERELOAD, 1.0).
-define(BASEBULLETSPEED, 8.0).
-define(BASEBULLETRELOAD, 4.0).
-define(DECAYBULLETSPEED, 0.5).
-define(DECAYBULLETRELOAD, 0.4).

%%% Cria um novo player com posição aleatória e velocidade zero (alterar para ser posiçoes opostas ) ??
newPlayer(Id) ->
    IdP = Id,
    Color = 50 + rand:uniform(205),
    Velocity = {0.0, 0.0},
    Position = case Id of
        1 -> {float(rand:uniform(200)), float(rand:uniform(700))};         % Lado esquerdo
        2 -> {float(1100 + rand:uniform(200)), float(rand:uniform(700))}   % Lado direito
    end,
    Score = 0,
    BulletSpeed = 8.0,
    BulletReload = 4.0,
    {IdP, Position, Velocity, Color, Score, BulletSpeed, BulletReload}.


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

update_player_decay({{IdP, Pos, Vel, Color, Score, BS, BR}, UserData})->
    if 
        BS > ?BASEBULLETSPEED , BR < ?BASEBULLETRELOAD ->
            NewBS = BS - ?DECAYBULLETSPEED,
            NewBR = BR + ?DECAYBULLETRELOAD;
        BS > ?BASEBULLETSPEED , BR == ?BASEBULLETRELOAD ->
            NewBS = BS - ?DECAYBULLETSPEED,
            NewBR = BR;
        BS == ?BASEBULLETSPEED, BR < ?BASEBULLETRELOAD ->
            NewBS = BS,
            NewBR = BR + ?DECAYBULLETRELOAD;
        BS == ?BASEBULLETSPEED, BR == ?BASEBULLETRELOAD ->
            NewBS = BS,
            NewBR = BR;
        BS < ?BASEBULLETSPEED , BR > ?BASEBULLETRELOAD ->
            NewBS = BS + ?DECAYBULLETSPEED,
            NewBR = BR - ?DECAYBULLETRELOAD;
        BS < ?BASEBULLETSPEED , BR == ?BASEBULLETRELOAD ->
            NewBS = BS + ?DECAYBULLETSPEED,
            NewBR = BR;
        BS == ?BASEBULLETSPEED, BR > ?BASEBULLETRELOAD ->
            NewBS = BS,
            NewBR = BR - ?DECAYBULLETRELOAD
    end,
    {{IdP, Pos, Vel, Color, Score, NewBS, NewBR}, UserData}.


update_player_position_reset({{IdP, _, Vel, Color, Score, BS, BR}, UserData}) ->
    NewPos = case IdP of
        1 -> {float(rand:uniform(200)), float(rand:uniform(700))};         % Lado esquerdo
        2 -> {float(1100 + rand:uniform(200)), float(rand:uniform(700))}   % Lado direito
    end,
    {{IdP, NewPos, Vel, Color, Score, BS, BR}, UserData}.

update_players_position_reset(Players) ->
    [update_player_position_reset(Player) || Player <- Players].

update_player_position({{IdP, Pos, Vel, Color, Score, BS, BR}, UserData}, Key) ->

    {Ax,Ay}= movement_to_acceleration(Key),
    {Vx, Vy} = Vel,
    NewVx = clamp(Vx + Ax, ?MAX_SPEED),
    NewVy = clamp(Vy + Ay, ?MAX_SPEED),
    {X, Y} = Pos,
    NewX = X + NewVx,
    NewY = Y + NewVy,
    
    {{IdP, {NewX, NewY}, {NewVx, NewVy}, Color, Score, BS, BR}, UserData}.

%%% Atualiza os buffs do player
update_player_modifier({{IdP, Pos, Vel, Color, Score, BS, BR}, UserData}, {_, _, Type, _}) ->
    case Type of
        green ->
            NewBS = BS + ?BULLETCHANGESPEED,
            NewBR = BR;
        orange ->
            NewBS = BS - ?BULLETCHANGESPEED,
            NewBR = BR;
        blue ->
            NewBS = BS,
            NewBR = BR + ?BULLETCHANGERELOAD;
        red ->
            NewBS = BS,
            NewBR = BR - ?BULLETCHANGERELOAD
    end,
    
    {{IdP, Pos, Vel, Color, Score, NewBS, NewBR}, UserData}.

%%% Atualiza o score da partida
update_player_score({{IdP, Pos, Vel, Color, Score, BS, BR}, UserData}, ChangedScore) ->

    NewScore = Score + ChangedScore,
    {{IdP, Pos, Vel, Color, NewScore, BS, BR}, UserData}.

update_players_decay([])->
    [];
update_players_decay(ListaJogadores) ->
    [update_player_decay(Jogador) || Jogador <- ListaJogadores].