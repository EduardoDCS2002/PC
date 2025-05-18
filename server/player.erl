-module(player).
-export([newPlayer/1, update_player_decay/1, update_players_decay/1, 
    update_player_reset/1, update_players_reset/1, update_player_position/2,
    update_player_modifier/2, update_player_score/2 ]).
-define(MAX_SPEED, 5.0).
-define(ACCELERATION, 0.2).
-define(BULLETCHANGESPEED, 2.0).
-define(BULLETCHANGERELOAD, 1.0).
-define(BASEBULLETSPEED, 8.0).
-define(BASEBULLETRELOAD, 4.0).
-define(DECAYBULLETSPEED, 0.01).
-define(DECAYBULLETRELOAD, 0.01).

%%% Cria um novo player com posição aleatória e velocidade zero (alterar para ser posiçoes opostas ) ??
newPlayer(Id) ->
    IdP = Id,
    Color = 50 + rand:uniform(205),
    Velocity = {0.0, 0.0},
    Position = case Id of
        1 -> {325.0, 350.0};         % Lado esquerdo
        2 -> {975.0, 350.0}   % Lado direito
    end,
    Score = 0,
    BulletSpeed = 8.0,
    BulletReload = 4.0,
    {IdP, Position, Velocity, Color, Score, BulletSpeed, BulletReload}.

%%Vai atualizando os valores alterados pelos modificadores
update_player_decay(Jogador)->
    {{IdP, Pos, Vel, Color, Score, BS, BR}, UserData} = Jogador,

    NewBS = case BS of
        _ when BS > ?BASEBULLETSPEED -> BS - ?DECAYBULLETSPEED;
        _ when BS < ?BASEBULLETSPEED -> BS + ?DECAYBULLETSPEED;
        _ -> BS
    end,

    NewBR = case BR of
        _ when BR > ?BASEBULLETRELOAD -> BR - ?DECAYBULLETRELOAD;
        _ when BR < ?BASEBULLETRELOAD -> BR + ?DECAYBULLETRELOAD;
        _ -> BR
    end,

    

    {{IdP, Pos, Vel, Color, Score, NewBS, NewBR}, UserData}.

update_players_decay([])->
    [];
update_players_decay(Jogadores) ->
    [update_player_decay(Jogador) || Jogador <- Jogadores].

update_player_reset(Jogador) ->
    {{IdP, _, Vel, Color, Score, BS, BR}, UserData} = Jogador,
    NewPos = case IdP of
        1 -> {325.0, 350.0};         % Lado esquerdo
        2 -> {975.0, 350.0}   % Lado direito
    end,
    NewVelo = {0.0,0.0},
    {{IdP, NewPos, NewVelo, Color, Score, BS, BR}, UserData}.

update_players_reset([])->
    [];
update_players_reset(Jogadores) ->
    [update_player_reset(Jogador) || Jogador <- Jogadores].


%%% Limita a velocidade máxima
clamp(V, Max) when V > Max -> Max;
clamp(V, Max) when V < -Max -> -Max;
clamp(V, _) -> V.

update_player_position({{IdP, Pos, Vel, Color, Score, BS, BR}, UserData}, Key) ->
    {Ax,Ay}= case Key of
        up -> {0.0, -?ACCELERATION};
        down -> {0.0, ?ACCELERATION};
        left -> {-?ACCELERATION, 0.0};
        right -> {?ACCELERATION, 0.0};
        _ -> {0.0, 0.0}
    end,
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

