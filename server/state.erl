-module (state).
-export ([start_state/0, update/1]).
-import(modifier, [new_modifier/0, update_modifiers/1, remove_modifier/2]).
-import(player, [newPlayer/0, update_player_position/2]).
-import(collision, [check_collisions_modifiers/2, check_collisions_bullet/2, distance/2, 
         collision_modifier/2, collision_bullet/2,
         check_colision_boards_players/1, check_colision_boards_bullet/1,
         borda/1]).
-import(projectile,[new_projectile/2, update_projectiles/1]).
-import (timer, [send_after/3]).%% verificar o estado do jogo
-import (conversor, [formatState/1, formataTecla/1]).


start_state() ->
    io:format(" New state~n"),
    register(game,spawn( fun() -> gameManager (novoEstado()) end )),
    Timer = spawn( fun() -> refresh(game) end),
    Salas = criaSalas(),
    register(statePid,spawn( fun() -> lounge(Salas)  end)).

refresh (Pid) -> receive after 10 -> Pid ! {refresh, self()}, refresh(Pid) end. % every 10 miliseconds will send the refresh signal to Pid

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
ondEstaJogador([], _) -> []; 

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
    {_,ListaJogadores} = H, 

    if length(ListaJogadores) < 2 -> 
        H;
        length(ListaJogadores) == 2 -> 
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
                length(Espera_Jogadores) < 1 ->
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
    State = {[], [],[], {1300,700}},
    io:fwrite("Estado novo Gerado.~n"),
    State.

adicionaJogador(Estado,Jogador) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra} = Estado,
    NovaListaJogadores=
        case length(ListaJogadores) of
            0 -> ListaJogadores ++ [{player:newPlayer(1), Jogador}];
            1 -> ListaJogadores ++ [{player:newPlayer(2), Jogador}];
            2 -> ListaJogadores
        end,
    State = { NovaListaJogadores , ListaModifiers, ListaBullets, TamanhoEcra},
    io:fwrite("Estado: ~p ~n", [State]),
    State.

removeJogador(Estado,Jogador) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra} = Estado,
    State = { ListaJogadores -- [Jogador], ListaModifiers, ListaBullets, TamanhoEcra},
    io:fwrite("Estado com jogador removido: ~p ~n", [State]),
    State.

%Controla tudo o que se passa no jogo 
gameManager(Estado)->
    receive
        {geraJogador, From} ->
            io:format("Gerar jogador~n"),
            %% Envia mensagem {game_over} após 2 minutos
            timer:send_after(120000, self(), game_over),    
            gameManager(adicionaJogador(Estado,From));
        

        %Recebe os argumentos de movimentação e atualiza a posição dos jogadores
        {movePlayer, Data, From} ->
            {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra} = Estado,

            % Encontrar e atualizar o jogador
            NovaListaJogadores = lists:map(
                fun({PlayerCordinates, {Username, Pid}} = Jogador) ->
                    case Pid of
                        From ->
                            NovoJogador = player:update_player_position({PlayerCordinates, {Username, Pid}}, Data),
                            NovoJogador;
                        _ ->
                            Jogador
                    end
                end,
                ListaJogadores
            ),

            NovoEstado = {NovaListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra},

            gameManager(NovoEstado);

        %Recebe os argumentos de shooting e cria the bullet
        {shoot, Data, From} ->
            {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra} = Estado,

            % Criar a Bala
            BulletsCriadas = lists:filtermap(
                %%% nao esta direito, brain lag , precisa de receber as cordenadas certas
                fun({{{PosX, PosY}, _, _, _, BulletSpeed, _}, {_, Pid}}) ->
                    case Pid of
                        From ->
                            NovaBala = projectile:new_projectile({PosX, PosY}, Data, BulletSpeed), % Data aqui deve ser {CursorX, CursorY}
                            NovaBala;
                        _ ->
                            false % deve dar skip right?
                    end
                end,
                ListaJogadores
            ),
            NovaListaBullets = BulletsCriadas ++ ListaBullets,
            NovoEstado = {ListaJogadores, ListaModifiers, NovaListaBullets, TamanhoEcra},

            gameManager(NovoEstado);

        {refresh, _} ->

            NovoEstado = update(Estado),
            {ListaJogadores,_, _, _} = Estado,
            Pids = [Pid || {_, {User, Pid}} <- ListaJogadores ],
            [ H ! {line,formatState(NovoEstado)} || H <- Pids],
            gameManager(NovoEstado);

       {leave, From} ->
            io:format("Alguem enviou leave~n"),
            {ListaJogadores, _ , _, _} = Estado,
            if
                length(ListaJogadores) == 1->
                    [H|T] = ListaJogadores,
                    {_, {_, Pid1}} = H,
                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H));
                        true ->
                            gameManager(Estado)
                    end;
                length(ListaJogadores) == 2 ->
                    [H1,H2 | T] = ListaJogadores,
                    {_, {_, Pid1}} = H1,
                    {_, {_, Pid2}} = H2,
                    if
                        Pid1 == From ->
                            gameManager(removeJogador(Estado,H1));
                        Pid2 == From ->
                            gameManager(removeJogador(Estado,H2));
                        true ->
                            gameManager(Estado)
                    end
            end

    end.



%Verifica as colisoes, atualiza todos os objectos e verifica tambem se alguem ganhou ou perdeu 
update(Estado) ->
    {ListaJogadores, ListaModifiers, ListaBullets, TamanhoEcra} = Estado,
    
    NewModifiers = modifier:update_modifiers(ListaModifiers),
    NewBullets = projectile:update_projectiles(ListaBullets),
    NewPlayers = player:update_players_decay(ListaJogadores),

    % Verificação de colisões
    CollisionsModifiers = collision:check_collisions_modifiers(NewPlayers, NewModifiers),
    PlayersAfterModifiers = handle_modifier_collisions(CollisionsModifiers),
    
    CollisionsBullets = collision:check_collisions_bullet(PlayersAfterModifiers, NewBullets),
    PlayersAfterBullets = handle_bullet_collisions(CollisionsBullets, PlayersAfterModifiers), 
    
    CollisionsBorder = collision:check_colision_boards_players(PlayersAfterBullets),
    PlayersAfterBorders = handle_bordas(CollisionsBorder, PlayersAfterBullets),


    %%something is wrong here, penso que o fullyupdatePlayer tinha de ser o bullet collision
    %%FullyUpdatedPlayers = handle_player_collisions(CollisionsAstronauts,NewAstronauts),
    verify_victory(Estado),
    {PlayersAfterBorders, NewModifiers, NewBullets, TamanhoEcra}.

%% still didnt finish
verify_victory(Estado) ->
    {ListaJogadores, _, _, _} = Estado,
    %% passado 2 minutos
    case length(ListaJogadores) of
        1 ->
            {ok, NewTimerRef} = timer:send_after(5000, self(), {timeout}),
            receive
                {timeout} ->
                    if length(ListaJogadores) == 1 ->
                        {_, {Username, Pid}} = hd(ListaJogadores),
                        handle_win(Username, Pid),
                        io:format("~p ganhou~n", [Username]),
                        game ! {leave, Pid};
                    true -> ok
                    end
            after 0 -> ok
            end;
        _ ->
            ok
    end.

handle_bordas([], Players) ->
    Players;
handle_bordas(PlayersCollided, AllPlayers) ->
    NotChangedPlayers = [Player || {{PId, _, _, _, _, _, _}, _} <- PlayersCollided, Player = {{IdP, _, _, _, _, _, _}, _} <- AllPlayers, PId =:= IdP],
    ChangedPlayers = [player:update_player_score(Player, 2) || {{PId, _, _, _, _, _, _}, _} <- PlayersCollided, Player = {{IdP, _, _, _, _, _, _}, _} <- AllPlayers, PId =/= IdP],
    player:update_players_position_reset(NotChangedPlayers ++ ChangedPlayers).

handle_loss(User,Pid) ->
    Pid ! {line,"Perdeu\n"},
    login_manager:logout(User),
    login_manager:update_loss(User).

handle_win(User,Pid) ->
    Pid ! {line,"Venceu\n"},
    login_manager:logout(User),
    login_manager:update_win(User).

handle_bullet_collisions([], Players) ->
    Players;
handle_bullet_collisions([{{{IdP, _, _, _, _, _, _}, _}, _} | Rest], Players) ->
    io:format("Collision between bullet ~p and player ~p~n", [IdP, Players]),
    
    NotChangedPlayers = [Player || Player = {{PId, _, _, _, _, _, _}, _} <- Players, PId =:= IdP],
    ChangeScorePlayers = [player:update_player_score(Player, 1) || Player = {{PId, _, _, _, _, _, _}, _} <- Players, PId =/= IdP],
    handle_bullet_collisions(Rest, NotChangedPlayers ++ ChangeScorePlayers).

%%Aplicar os efeitos dos modificadores
handle_modifier_collisions([]) ->
    [];
handle_modifier_collisions([{Player, Modifier} | Rest]) ->
    io:format("Colisão com um modificador ~p e ~p~n", [Player, Modifier]),
    [player:update_player_modifier(Player, Modifier) | handle_modifier_collisions(Rest)].

handle_bordas([]) ->
    [];
handle_bordas([Player | Players]) -> % precisa de atualizar o score do Player e devolver o novo valor
    {_,{U,Pid}} = Player,
    io:format("O jogador ~p bateu na borda.~n",[Player]),
    %%handle_loss(U,Pid),
    %%game ! {leave, Pid},
    handle_bordas(Players).