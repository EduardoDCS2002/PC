-module (state).
-export ([refresh_loop/2,gameManager/1,start_state/0,start_game_loop/0,game_loop/0, update/1,adicionaJogador/2, handle_loss/2,handle_win/2,handle_bordas_collisions/2,handle_bullet_collisions/3,handle_modifier_collisions/3]).
-import(modifier, [new_modifier/1, maybe_spawn_modifier/1, update_modifiers/1, remove_modifier/2]).
-import(player, [newPlayer/1, update_player_decay/1, update_players_decay/1, 
    update_player_reset/1, update_players_reset/1, update_player_position/2,
    update_player_modifier/2, update_player_score/2 ]).
-import(collision, [check_collisions_modifiers/2, check_collisions_bullet/2, distance/2, 
         collision_modifier/2, collision_bullet/2,
         check_colision_boards_players/1, check_colision_boards_bullets/1,
         borda/1]).
-import(projectile,[new_projectile/3, update_projectiles/1, update_projectile/1, remove_bullets/2]).
-import (timer, [send_after/3]).%% verificar o estado do jogo
-import (conversor, [formatState/1, formataTecla/1]).

%ANTIGA FUNCIONAL
%start_state() ->
 %   io:format(" New state~n"),
  %  register(game,spawn( fun() -> gameManager (novoEstado()) end )),
   % Timer = spawn( fun() -> refresh(game) end),
    %Salas = criaSalas(),
    %register(statePid,spawn( fun() -> lounge(Salas)  end)).


start_state() ->
    io:format("New state~n"),
    spawn(fun() -> start_game_loop() end),
    Salas = criaSalas(),
    register(statePid, spawn(fun() -> lounge(Salas) end)).

start_game_loop() ->
    spawn(fun game_loop/0).

game_loop() ->
    io:format("Iniciando novo gameManager e refresh~n"),
    % Cria gameManager
    GamePid = spawn(fun() -> gameManager(novoEstado()) end),
    register(game, GamePid),

    % Cria refresh que monitora gameManager
    RefresherPid = spawn(fun() -> refresh(GamePid) end),

    % Espera o gameManager morrer (usando monitor)
    Ref = erlang:monitor(process, GamePid),
    receive
        {'DOWN', Ref, process, GamePid, Reason} ->
            io:format("GameManager terminou: ~p~n", [Reason]),
            % Mata refresh (se não morreu automaticamente)
            RefresherPid ! stop,
            % Aqui pode limpar estado, salas, etc se quiser
            % Reinicia o loop para um novo jogo
            game_loop()
    end.

%%NOVO
refresh(Pid) ->
    Ref = erlang:monitor(process, Pid),
    refresh_loop(Pid, Ref).

refresh_loop(Pid, Ref) ->
    receive
        stop -> 
            ok;
        {'DOWN', Ref, process, Pid, Reason} -> 
            io:format("GameManager terminou com motivo: ~p~n", [Reason]),
            ok % termina o refresh
    after 10 -> 
        Pid ! {refresh, self()}, 
        refresh_loop(Pid, Ref)
    end.

%refresh (Pid) -> 
    %tirar o stop ok para ter a versao antiga
    %receive 
    %    stop -> ok;
   % after 10 -> Pid ! {refresh, self()}, refresh(Pid) end. % every 10 miliseconds will send the refresh signal to Pid

novaSala() -> 
    [{spawn(fun() -> estado([],[]) end),[]}].

%Cria 4 salas 
criaSalas() -> 
    novaSala() ++ novaSala() ++ novaSala() ++ novaSala(). 

lounge(Salas) -> 

    receive 
        {ready,Username, UserProcess} -> 

            Sala = verificaSala(Salas), % returns a room with space
            {Pid,ListaJogadores} = Sala,
            NovasSalas = remove(Sala,Salas),
            Pid ! {ready,Username,UserProcess},

            NovoJogadores = ListaJogadores ++ [{Username,UserProcess}],
            NovoSala = {Pid,NovoJogadores},

            lounge([NovoSala | NovasSalas]);

        {leave, Username, UserProcess}  ->                                            
            ondEstaJogador(Salas,UserProcess) ! {leave,Username,UserProcess},
            lounge(Salas)
    end. 

remove(X, L) ->
    [Y || Y <- L, Y =/= X].


%Encontra o Jogador na sala onde ele se encontra
ondEstaJogador([], UserProcess) -> []; 

ondEstaJogador([{Pid,[X|Y]}|T],UserProcess) -> 
    {User,Process} = X, 

    if Process == UserProcess ->
        Pid;
    true -> 
        if length(Y) == 0 -> 
            ondEstaJogador([T],UserProcess);
        true-> 
            ondEstaJogador([{Pid,Y}|T],UserProcess)
        end
    end.

%Verifica se a sala esta cheia ou nao 
%Se estiver entao vai para a sala seguinte 

verificaSala([H|T]) -> 
    {Pid,ListaJogadores} = H, 

    if length(ListaJogadores) < 2 -> 
        H;
    true ->
        verificaSala(T)
    end.


esperar_jogadores(Espera_Jogadores, TimerRef) ->
    receive
        {ready, Username, UserProcess} ->
            % Cancela o temporizador atual
            timer:cancel(TimerRef),
            Espera_JogadoresNovo = Espera_Jogadores ++ [{Username, UserProcess}],
            % Define um novo temporizador de 5 segundos
            {ok, NewTimerRef} = timer:send_after(5000, self(), {timeout}),
            esperar_jogadores(Espera_JogadoresNovo, NewTimerRef);

        {timeout} ->
            % Quando o tempo expira, inicia o jogo se houver jogadores suficientes
            if length(Espera_Jogadores) == 2 ->
                [JogadorPid ! {comeca, game} || {_, JogadorPid} <- Espera_Jogadores],
                [game ! {geraJogador, {Username, UserProcess}} || {Username, UserProcess} <- Espera_Jogadores],
                estado(Espera_Jogadores, []);
            true ->
                % Se não houver jogadores suficientes, continua esperando
                io:format("Não há jogadores suficientes, esperando mais~n"),
                {ok, NewTimerRef} = timer:send_after(10000, self(), {timeout}),
                esperar_jogadores(Espera_Jogadores, NewTimerRef)
            end;

        {leave, Username, UserProcess} ->
            Espera_JogadoresNovo = lists:delete({Username, UserProcess}, Espera_Jogadores),
            esperar_jogadores(Espera_JogadoresNovo, TimerRef)
    end.


estado(Atuais_Jogadores, Espera_Jogadores) ->
    io:format("Entrei no estado ~n"),
    receive
        {ready, Username, UserProcess} ->
            io:format("len ~p ~n", [length(Espera_Jogadores)]),
            if
                length(Espera_Jogadores) < 2 ->
                    io:format("Recebi ready de ~p mas ele vai esperar ~n", [Username]),
                    Espera_JogadoresNovo = Espera_Jogadores ++ [{Username, UserProcess}],
                    {ok, TimerRef} = timer:send_after(10000, self(), {timeout}),
                    esperar_jogadores(Espera_JogadoresNovo, TimerRef);
                true ->
                    io:format("Recebi ready do User ~p e vou adicionar-lo ao jogo ~n", [Username]),
                    Espera_JogadoresAux = Espera_Jogadores ++ [{Username, UserProcess}],
                    [JogadorPid ! {comeca, game} || {_, JogadorPid} <- Espera_JogadoresAux],
                    [game ! {geraJogador, {Username, UserProcess}} || {Username, UserProcess} <- Espera_JogadoresAux],
                    estado(Espera_JogadoresAux, [])
            end;

        {leave, Username, UserProcess} ->
            io:format("Recebi leave do User ~p ~n", [Username]),
            case length(Espera_Jogadores) of
                0 ->
                    Lista = Atuais_Jogadores -- [{Username, UserProcess}],
                    io:format("CASE 0 - A lista de jogadores ativos atuais ~p ~n", [Lista]),
                    estado(Lista, Espera_Jogadores);
                _ ->
                    io:format("A lista de jogadores ativos atuais ~p ~n", [Atuais_Jogadores]),
                    io:format("Vou tirar o ~p da lista ~n", [{Username, UserProcess}]),
                    Lista = Atuais_Jogadores -- [{Username, UserProcess}],
                    io:format("Lista jogador removido ~p ~n", [Lista]),
                    if
                        length(Espera_Jogadores) > 0 ->
                            [H | T] = Espera_Jogadores,
                            {_, UP} = H,
                            UP ! {comeca, game},
                            game ! {geraJogador, H},
                            ListaA = Lista ++ [H];
                        true ->
                            T = [],
                            ListaA = Lista
                    end,
                    estado(ListaA, T)
            end
    end.


novoEstado() ->
    %player, modifiers, bullets, screensize
    StartTime = erlang:monotonic_time(millisecond),
    State = {[], [],[], {1300,700}, StartTime},
    io:fwrite("Estado novo Gerado.~n"),
    State.

adicionaJogador(Estado,Jogador) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime} = Estado,
    NovaListaJogadores=
        case length(ListaJogadores) of
            0 -> ListaJogadores ++ [{player:newPlayer(1), Jogador}];
            1 -> ListaJogadores ++ [{player:newPlayer(2), Jogador}];
            _ -> ListaJogadores
        end,
    State = { NovaListaJogadores , ListaModifiers, ListaBullets, TamanhoEcra, StartTime},
    io:fwrite("Estado: ~p ~n", [State]),
    State.

removeJogador(Estado,Jogador) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime} = Estado,
    State = { ListaJogadores -- [Jogador], ListaModifiers, ListaBullets, TamanhoEcra, StartTime},
    io:fwrite("Estado com jogador removido: ~p ~n", [State]),
    State.

%Controla tudo o que se passa no jogo 
gameManager(Estado)->
    receive
        {geraJogador, From} ->
            io:format("Gerar jogador~n"),   
            gameManager(adicionaJogador(Estado,From));
        

        %Recebe os argumentos de movimentação e atualiza a posição dos jogadores
        {keyPressed, Dir, From} ->
            {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime} = Estado,

            % Encontrar e atualizar o jogador
            io:format("Recebido comando: ~p~n", [Dir]),
            NovaListaJogadores = lists:map(
                fun({PlayerCordinates, {Username, Pid}} = Jogador) ->
                    case Pid of
                        From ->
                            NovoJogador = player:update_player_position({PlayerCordinates, {Username, Pid}}, Dir),
                            NovoJogador;
                        _ ->
                            Jogador
                    end
                end,
                ListaJogadores
            ),

            NovoEstado = {NovaListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime},

            gameManager(NovoEstado);

        %Recebe os argumentos de shooting e cria the bullet
        {shoot, Pos, From} ->
            {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime} = Estado,
            io:format("From recebido: ~p~n", [From]),
            %lists:foreach(fun({_, {_, Pid}}) -> io:format("Comparar com Pid: ~p~n", [Pid]) end, ListaJogadores),
            %io:format("ListaJogadores:~n~p~n", [ListaJogadores]),
            % Criar a Bala
            TimeNow = erlang:monotonic_time(millisecond),

            {UpdatedJogadores, BulletsCriadas} = lists:foldl(
                fun(Jogador, {AccJogadores, AccBullets}) ->
                    {{IdP, {X,Y}, V, S, C, BS, BR, NB}, {Username, Pid}} = Jogador,
                    case From of
                        Pid ->
                            TimeDiff = TimeNow - NB,
                            ReloadTime = trunc(BR * 1000),

                            if
                                TimeDiff >= ReloadTime ->
                                    io:format("Match encontrado! Criar bala para jogador: ~p~n", [Username]),
                                    NovaBala = projectile:new_projectile({X, Y}, Pos, BS),

                                    % Atualizar o jogador com novo NextBullet = TimeNow
                                    JogadorAtualizado = {
                                        {IdP, {X,Y}, V, S, C, BS, BR, TimeNow},
                                        {Username, Pid}
                                    },

                                    {
                                        [JogadorAtualizado | AccJogadores],
                                        [NovaBala | AccBullets]
                                    };
                                true ->
                                    io:format("Jogador ~p ainda a recarregar: ~p/~p~n", [Username, TimeDiff, ReloadTime]),
                                    {[Jogador | AccJogadores], AccBullets}
                            end;
                        _ ->
                            {[Jogador | AccJogadores], AccBullets}
                    end
                end,
                {[], []},
                ListaJogadores
            ),

            NovaListaBullets = lists:reverse(BulletsCriadas) ++ ListaBullets,
            NovaListaJogadores = lists:reverse(UpdatedJogadores),
            NovoEstado = {NovaListaJogadores, ListaModifiers, NovaListaBullets, TamanhoEcra, StartTime},

            gameManager(NovoEstado);

        {refresh, _} ->

            NovoEstado = update(Estado),
            {ListaJogadores,_, _, _, _} = NovoEstado,
            Pids = [Pid || {_, {User, Pid}} <- ListaJogadores ],
            [ H ! {line,formatState(NovoEstado)} || H <- Pids],
            gameManager(NovoEstado);

       {leave, From} ->
            io:format("Alguem enviou leave~n"),
            {ListaJogadores, _ , _, _, _} = Estado,
            %%%VERSAO ANTIGA
            %length(ListaJogadores) == 2 ->
                    %[H1,H2 | T] = ListaJogadores,
                    %{_, {_, Pid1}} = H1,
                    %{_, {_, Pid2}} = H2,
                    %if
                       % Pid1 == From ->
                            %gameManager(removeJogador(Estado,H1));
                       % Pid2 == From ->
                            %gameManager(removeJogador(Estado,H2));
                       % true ->
                           % gameManager(Estado)
                   % end
            [H1,H2 | T] = ListaJogadores,
            {_, {_, Pid1}} = H1,
            {_, {_, Pid2}} = H2,
            Pendente = removeJogador(Estado,H1),
            Final = removeJogador(Pendente,H2),
            exit(normal) % <-- MATA o processo gameManager



    end.



%Verifica as colisoes, atualiza todos os objectos e verifica tambem se alguem ganhou ou perdeu 
update(Estado) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra, StartTime} = Estado,
    
    NewModifiers = modifier:update_modifiers(ListaModifiers),
    NewBullets = projectile:update_projectiles(ListaBullets),
    NewPlayers = player:update_players_decay(ListaJogadores),

    % Verificação de colisões
    %retorna a lista dos jogadores que bateram contra algo
    CollisionsModifiers = collision:check_collisions_modifiers(NewPlayers, NewModifiers),
    {PlayersAfterModifiers, ModifiersAfterCollisions} = handle_modifier_collisions(NewPlayers, NewModifiers, CollisionsModifiers),
    
    CollisionsBullets = collision:check_collisions_bullet(PlayersAfterModifiers, NewBullets),
    {PlayersAfterBullets,BulletsAfterCollisions} = handle_bullet_collisions( PlayersAfterModifiers, NewBullets, CollisionsBullets), 

    CollisionsBulletsBorder = collision:check_colision_boards_bullets(BulletsAfterCollisions),
    BulletsAfterBorders = handle_bordas_bullets_collisions(BulletsAfterCollisions, CollisionsBulletsBorder),
    
    CollisionsBorder = collision:check_colision_boards_players(PlayersAfterBullets),
    PlayersAfterBorders = handle_bordas_collisions(PlayersAfterBullets, CollisionsBorder),

    %verify_victory({PlayersAfterBorders, ModifiersAfterCollisions, BulletsAfterCollisions, TamanhoEcra, StartTime}),
    verify_victory({PlayersAfterBorders, ModifiersAfterCollisions, BulletsAfterBorders, TamanhoEcra, StartTime}),
    %{PlayersAfterBorders, ModifiersAfterCollisions, BulletsAfterCollisions, TamanhoEcra, StartTime}.
    {PlayersAfterBorders,ModifiersAfterCollisions, BulletsAfterBorders, TamanhoEcra, StartTime}.

%% still didnt finish
verify_victory(Estado) ->
    {ListaJogadores, _, _, _, StartTime} = Estado,
    CurrentTime = erlang:monotonic_time(millisecond),
    TimeElapsed = CurrentTime - StartTime,
    %% passado 2 minutos
    if
        TimeElapsed >= 120000 ->
            [{{_, _, _, _, S1, _, _,_}, {U1, Pid1}}, {{_, _, _, _, S2, _, _, _}, {U2, Pid2}}] = ListaJogadores,
            case S1 > S2 of
                true ->
                    handle_win(U1, Pid1),
                    handle_loss(U2, Pid2),
                    game ! {leave, Pid1};
                    %game ! {leave, Pid2};
                false when S2 > S1 ->
                    handle_win(U2, Pid2),
                    handle_loss(U1, Pid1),
                    game ! {leave, Pid1};
                   %game ! {leave, Pid2};
                false ->
                    %% empate - ninguém ganha ou perde, mas ambos saem
                    Pid1 ! {line, "Empate\n"},
                    Pid2 ! {line, "Empate\n"},
                    game ! {leave, Pid1}
                    %game ! {leave, Pid2}
            end;
        true ->
            ok
    end.

handle_loss(User,Pid) ->
    Pid ! {line,"Perdeu\n"},
    login_manager:logout(User),
    login_manager:update_loss(User).

handle_win(User,Pid) ->
    Pid ! {line,"Venceu\n"},
    login_manager:logout(User),
    login_manager:update_win(User).

handle_bordas_collisions(Players, CollisionsBoarders) ->
   % Obter lista de IDs dos jogadores que colidiram com a borda
    CollidedIds = [Id || {{Id, _, _, _, _, _, _, _}, _} <- CollisionsBoarders],

    case CollidedIds of
        [] ->
            UpdatedPlayers2 = Players; % ninguém colidiu, não dá pontos
        _  ->
            % Atualiza apenas quem não colidiu
            UpdatedPlayers = [maybe_update_score(Player, CollidedIds, 2) || Player <- Players],
            UpdatedPlayers2 = player:update_players_reset(UpdatedPlayers)
    end,
    
    UpdatedPlayers2.

handle_bordas_bullets_collisions(Bullets, CollisionsBulletsBorder) ->
   % Obter lista de IDs dos jogadores que colidiram com a borda
   case CollisionsBulletsBorder of
        [] -> ok;
        _  -> io:format("Colisões da bala com a bord: ~p~n", [CollisionsBulletsBorder])
    end,
   CollidedPositions = [Pos || {{Pos, _, _, _}} <- CollisionsBulletsBorder],

    case CollidedPositions of
        [] ->
            UpdateBullets = Bullets; % ninguém colidiu, não dá pontos
        _  ->
            % Atualiza apenas quem não colidiu
            UpdateBullets = projectile:remove_bullets(CollidedPositions, Bullets)
    end,
    
    UpdateBullets.

maybe_update_score(Player, CollidedIds, Points) ->
    {{Id, _, _, _, _, _, _,_}, _} = Player,
    case lists:member(Id, CollidedIds) of
        true -> Player; % não atualiza se colidiu
        false -> player:update_player_score(Player, Points) % atualiza se não colidiu
    end.

handle_bullet_collisions(Players, Bullets, CollisionsBullets) ->
    case CollisionsBullets of
        [] -> ok;
        _  -> io:format("Colisões com bala: ~p~n", [CollisionsBullets])
    end,
    CollidedIds = [Id || {{{Id, _, _, _, _, _, _, _}, _}, Projectile} <- CollisionsBullets],

    case CollidedIds of
        [] ->
            UpdatedPlayers = Players,
            NewBullets = Bullets; % ninguém colidiu, não dá pontos
        _  ->
            % Atualiza apenas quem não colidiu
            UpdatedPlayers = [maybe_update_score(Player, CollidedIds, 1) || Player <- Players],
            CollidedPositions = [Pos || {_, {Pos, _, _, _}} <- CollisionsBullets],
            % Remover modificadores colididos
            NewBullets = projectile:remove_bullets(CollidedPositions, Bullets)
            
    end,

    {UpdatedPlayers, NewBullets}.

maybe_update_buffs(Player, CollidedIds) ->
    {{Id, _, _, _, _, _, _, _}, _} = Player,
    case lists:keyfind(Id, 1, CollidedIds) of
        {Id, Modifier} ->
            player:update_player_modifier(Player, Modifier); % se colidiu, aplica
        false ->
            Player % se não colidiu com nenhum modifier, retorna como está
    end.

%%Aplicar os efeitos dos modificadores
handle_modifier_collisions(Players, Modifiers, CollisionsModifiers) ->
    case CollisionsModifiers of
        [] -> ok;
        _  -> io:format("Colisões com modifiers: ~p~n", [CollisionsModifiers])
    end,
    CollidedIds = [{Id,Modifier} || {{{Id, _, _, _, _, _, _, _}, _}, Modifier} <- CollisionsModifiers],

    case CollidedIds of
        [] ->
            UpdatedPlayers = Players,
            NewModifiers = Modifiers; % ninguém colidiu, não dá pontos
        _  ->
            % Atualiza apenas quem não colidiu
            UpdatedPlayers = [maybe_update_buffs(Player, CollidedIds) || Player <- Players],
            CollidedPositions = [Pos || {_, {Pos, _, _, _}} <- CollisionsModifiers],
            % Remover modificadores colididos
            NewModifiers = modifier:remove_modifier(CollidedPositions, Modifiers)
            
    end,
    
    {UpdatedPlayers, NewModifiers}.



