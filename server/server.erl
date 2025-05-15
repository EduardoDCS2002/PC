-module(server).
-export([start/0]).
-import(login_manager, [start_Login_Manager/1, create_account/2, close_account/2, login/2, logout/1]).
-import(state, [start_state/0]).


start() ->
    io:format("Server started~n"),
    % Start state process and monitor it
    StatePid = spawn(fun() -> state:start_state() end),
    register(state, StatePid),
    erlang:monitor(process, StatePid), % starts monitoring the state, if it crashes it will send a message
                                    %  to the server with this format {'DOWN', MonitorRef, process, StatePid, Reason}

    % Load logins or start fresh
    LoginMap = case file:consult("Logins.txt") of
        {ok, L} -> maps:from_list(L);
        {error, _} -> maps:new()
    end,

    % Start login_manager with monitoring
    {ok, Pid} = login_manager:start_link(LoginMap),
    erlang:monitor(process, Pid),

    % TCP setup with error handling
    Port = 22346,
    case gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]) of
        {ok, Socket} -> acceptor(Socket);
        {error, Reason} -> io:format("TCP listen failed: ~p~n", [Reason])
    end.

% Acceptor with crash handling
acceptor(Socket) ->
    case gen_tcp:accept(Socket) of
        {ok, Sock} ->
            spawn(fun() -> acceptor(Socket) end), % Parallel accept
            authenticator(Sock);
        {error, closed} -> io:format("Socket closed~n");
        {error, Reason} -> io:format("Accept error: ~p~n", [Reason])
    end.

% Authenticator: Parses commands securely
authenticator(Sock) ->
    receive
        {tcp, _, Data} ->
            case parse_command(binary_to_list(Data)) of
                {ok, Cmd, User, Pass} -> handle_command(Sock, Cmd, User, Pass);
                {error, Msg} -> gen_tcp:send(Sock, Msg)
            end,
            authenticator(Sock);
        {tcp_closed, _} -> io:format("Client disconnected~n");
        {tcp_error, _, Reason} -> io:format("TCP error: ~p~n", [Reason])
    end.

% Robust command parser (supports spaces in passwords)
parse_command(Data) ->
    Trimmed = string:trim(Data),
    case string:split(Trimmed, " ", leading) of
        [Cmd | Rest] ->
            case Cmd of
                "login" -> parse_credentials(Rest, "login");
                "create_account" -> parse_credentials(Rest, "create_account");
                "close_account" -> parse_credentials(Rest, "close_account");
                _ -> {error, <<"Invalid command\n">>}
            end;
        _ -> {error, <<"Empty command\n">>}
    end.

parse_credentials([], _Cmd) -> {error, <<"Missing credentials\n">>};
parse_credentials([UserPass], Cmd) ->
    %io:format("~s~n", UserPass),
    case string:split(UserPass, " ", trailing) of
        [User, Pass] -> {ok, Cmd, string:trim(User), string:trim(Pass)};
        _ -> {error, <<"Format: COMMAND USER:PASS\n">>}
    end.

% Command handler
handle_command(Sock, "login", User, Pass) ->
    case login(User, Pass) of
        ok -> 
            gen_tcp:send(Sock, <<"Login successful\n">>),
            user(Sock, User);
        _ -> 
            gen_tcp:send(Sock, <<"Login failed\n">>)
    end;
handle_command(Sock, "create_account", User, Pass) ->
    %io:format("~s ~s~n", User, Pass),
    case create_account(User, Pass) of
        ok -> gen_tcp:send(Sock, <<"Account created\n">>);
        bad_arguments -> gen_tcp:send(Sock, <<"Bad arguments">>);
        account_exists -> gen_tcp:send(Sock, <<"Account exists\n">>)
    end;
handle_command(Sock, "close_account", User, Pass) ->
    case close_account(User, Pass) of
        ok -> gen_tcp:send(Sock, <<"Account closed\n">>);
        _ -> gen_tcp:send(Sock, <<"Invalid credentials\n">>)
    end.

% User session with crash handling
user(Sock, Username) ->
    state ! {ready, Username, self()},
    gen_tcp:send(Sock, <<"Waiting for a game...\n">>),
    receive
        {comeca, GameManager} ->
            gen_tcp:send(Sock, <<"Game starts!\n">>),
            game_loop(Sock, Username, GameManager);
        {'DOWN', _, _, _, _} -> % Handle process crashes
            gen_tcp:send(Sock, <<"Server error\n">>)
    after 30000 -> % Timeout after 30s
        gen_tcp:send(Sock, <<"Timeout\n">>)
    end.

% Game loop with quit/error handling
game_loop(Sock, Username, GameManager) ->
    receive
        {line, Data} -> % From GameManager
            gen_tcp:send(Sock, Data),
            game_loop(Sock, Username, GameManager);
        {tcp, _, Data} -> % From client
            case string:trim(binary_to_list(Data)) of
                "quit" -> 
                    state ! {leave, Username, self()},
                    logout(Username),
                    gen_tcp:send(Sock, <<"Goodbye!\n">>);
                Input ->
                    GameManager ! {keyPressed, Input, self()},
                    game_loop(Sock, Username, GameManager)
            end;
        {tcp_closed, _} -> 
            state ! {leave, Username, self()},
            logout(Username);
        {tcp_error, _, _} -> 
            state ! {leave, Username, self()},
            logout(Username)
    end.