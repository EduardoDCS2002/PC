-module(server).
-export([start/0]).
-import(login_manager, [start_Login_Manager/1, create_account/2, close_account/2, login/2, logout/1]).
-import(state, [start_state/0]).


%Cria o servidor 
start () ->
    io:format("Iniciei o Server~n"),
    PidState = spawn ( fun() -> state:start_state() end),  %Iniciar o processo com o estado do servidor
    register(state,PidState),

    {_, L} = file:consult("Logins.txt"),
    Mapa = maps:from_list(L),

    register(login_manager, spawn( fun() -> login_manager:start_Login_Manager(Mapa) end)), % Login manager
    Port = 22345,
    {ok, Socket} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),    %Socket
    acceptor(Socket).

acceptor ( Socket )->
    {ok, Sock} = gen_tcp:accept(Socket),
    spawn( fun() -> acceptor( Socket ) end), 
    authenticator(Sock).

% Acceptor with crash handling
authenticator(Sock) ->
    io:format("Iniciei o Autenticador~n"),
    receive
        {tcp, _ , Data}->
            StrData = binary:bin_to_list(Data),
            %io:format("Recebi estes Dados~p~n",[StrData]),
            ListaDados = string:tokens(string:substr(StrData,1,(string:len(StrData)-2)), " "),
            LenghtListaDados = length(ListaDados),
            if 
                LenghtListaDados == 1 ->
                    [Acao | _ ] = ListaDados,
                    User = "",
                    Pass = "";
                LenghtListaDados == 2 ->
                    [Acao | Aux] = ListaDados,
                    [User | _ ] = Aux,
                    Pass = "";
                true ->
                    [Acao | Aux] = ListaDados,
                    [User | Passs] = Aux,
                    [Pass1 | _ ] = Passs,
                    Pass = Pass1
            end,

            case Acao of
                "login" when User =:= "" ->
                    io:format("Login Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Login Falhou User inválido\n">>),
                    authenticator(Sock);

                "login" when Pass =:= "" ->
                    io:format("Login Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Login Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "login" ->
                   
                    U = re:replace(User, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),                   
                    
                    case login(U,P) of
                        ok ->
                            io:format("Login Deu ~n"),
                            gen_tcp:send(Sock, <<"Login feito com sucesso!\n">>),
                            user(Sock, U);
                        _ ->
                            io:format("Login nao deu ~n"),
                            gen_tcp:send(Sock,<<"Erro ao fazer login!\n">>),
                            authenticator(Sock) % Volta a tentar autenticar-se
                    end;
                "create_account" when User =:= "" ->
                    io:format("Create Account Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Create Account Falhou User inválido\n">>),
                    authenticator(Sock);

                "create_account" when Pass =:= "" ->
                    io:format("Create Account Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Create Account Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "create_account" ->
                    
                    U = re:replace(User, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    case create_account(U,P) of
                        ok ->
                            io:format("Create Account feito com sucesso! ~n"),
                            gen_tcp:send(Sock, <<"Create Account feito com sucesso!\n">>),
                            %user(Sock, U);
                            authenticator(Sock);
                        _ ->
                            io:format("Username e Password não correspondem! ~n"),
                            gen_tcp:send(Sock,<<"Conta já existente!\n">>),
                            authenticator(Sock)
                    end;

                "close_account" when User =:= "" ->
                    io:format("Close Account Falhou User inválido ~n"),
                    gen_tcp:send(Sock,<<"Close Account Falhou User inválido \n">>),
                    authenticator(Sock);

                "close_account" when Pass =:= "" ->
                    io:format("Close Account Falhou Pass inválida ~n"),
                    gen_tcp:send(Sock,<<"Close Account Falhou Pass inválida\n">>),
                    authenticator(Sock);

                "close_account" ->
                    
                    U = re:replace(User, "(^\s+)|(\s+$)", "", [global,{return,list}]),
                    P = re:replace(Pass, "(^\s+)|(\s+$)", "", [global,{return,list}]),
                    case close_account(U,P) of
                        ok ->
                            io:format("Close Account feito com sucesso! ~n"),
                            gen_tcp:send(Sock, <<"Close Account feito com sucesso!\n">>),
                            %user(Sock, U);
                            authenticator(Sock);
                        _ ->
                            io:format("Username e Password não correspondem! ~n"),
                            gen_tcp:send(Sock,<<"Username e Password não correspondem!\n">>),
                            authenticator(Sock)
                    end;

                "ranking" ++ _Rest ->
                    case file:read_file("Logins.txt") of
                        {ok, Binary} ->
                            String = binary_to_list(Binary),
                            gen_tcp:send(Sock, String);
                        {error, Reason} ->
                            gen_tcp:send(Sock, "ERROR: " ++ file:format_error(Reason))
                    end;

                _ ->
                    gen_tcp:send(Sock,<<"Opção Inválida \n">>),
                    %io:format("dados ~p~n",[Data]),
                    authenticator(Sock)
            end
    end.

user(Sock, Username) ->
    statePid ! {ready, Username, self()},
    gen_tcp:send(Sock, <<"Há espera por vaga\n">>),
    io:format("À espera de uma mensagem \"comeca\"!~n"),
    receive % Enquanto não receber resposta fica bloqueado
        {comeca, GameManager} ->
            gen_tcp:send(Sock, <<"Comeca\n">>),
            io:format("Vou começar o jogo~n"),
            cicloJogo(Sock, Username, GameManager) % Desbloqueou vai para a função principal do jogo
    end.
%%new, tentar tratar os dados vindo dos controlos
parse_input(Data, From) ->
    io:format("PID: ~p mandou o comando: ~p~n", [From, Data]),
    % Remove espaços
    Clean = re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    Stripped = re:replace(Clean, "\r", "", [global,{return,list}]),
    % Quebra a string em tokens separados por espaço
    Tokens = string:tokens(Stripped, " "),
    case Tokens of
        ["quit"] ->
            quit;
        ["bang", Xstr, Ystr] ->
            {X,Y} = {string:to_integer(Xstr), string:to_integer(Ystr)},
            {{NumX,_},{NumY,_}}= {X,Y},
            case {NumX, NumY} of
                {NumX,NumY} ->
                    NewX = float(NumX),
                    NewY = float(NumY),
                    {shoot, {NewX,NewY}, From};
                _ ->
                    io:format("Erro ao converter para float: ~p~n", [Tokens]),
                    unknown_command
            end;
        ["L"] -> {keyPressed, left, From};
        ["U"] -> {keyPressed, up, From};
        ["R"] -> {keyPressed, right, From};
        ["D"] -> {keyPressed, down, From};
        _ ->
            unknown_command
    end.

cicloJogo(Sock, Username, GameManager) -> 
    receive
        {line, Data} -> % line é dados do game manager
            %io:format("ENVIEI ESTES DADOS~p~n",[Data]),
            gen_tcp:send(Sock, Data),
            cicloJogo(Sock, Username, GameManager);

        {tcp, _, Data} ->
            case parse_input(Data, self()) of
                quit ->
                    io:format("Recebi quit~n"),
                    statePid ! {leave, Username, self()},
                    logout(Username),
                    authenticator(Sock);
                {shoot, Pos, From} ->
                    GameManager ! {shoot, Pos, From},
                    cicloJogo(Sock, Username, GameManager);
                {keyPressed, Dir, From} ->
                    GameManager ! {keyPressed, Dir, From},
                    cicloJogo(Sock, Username, GameManager);
                unknown_command ->
                    io:format("Comando desconhecido recebido: ~p~n", [Data]),
                    cicloJogo(Sock, Username, GameManager)
            end;

        {tcp_closed, _} ->
            statePid ! {leave, Username, self()},
            logout(Username);
        {tcp_error, _} ->
            statePid ! {leave, Username, self()},
            logout(Username)
    end.