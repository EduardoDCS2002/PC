-module(login_manager).
-export([start_Login_Manager/1, create_account/2, close_account/2, login/2, logout/1, 
         mapa_para_string/1, maps_para_string/1, booleanoString/1,
         update_loss/1, update_win/1]).

% Atomic file save helper
save_map(Map) ->
    Data = maps_para_string(maps:to_list(Map)),
    file:write_file("Logins.txt", Data, [write]).

start_Login_Manager(Mapa) ->
    Pid = spawn(fun() -> loop(Mapa) end),
    register(login_manager, Pid).

call(Request) ->
    login_manager ! {Request, self()},
    receive Res -> Res end.

update_loss(Username) -> call({update_loss, Username}).
update_win(Username) -> call({update_win, Username}).
create_account(Username, Passwd) -> call({create_account, Username, Passwd}).
close_account(Username, Passwd) -> call({close_account, Username, Passwd}).
login(Username, Passwd) -> call({login, Username, Passwd}).
logout(Username) -> call({logout, Username}).

booleanoString(true) -> "true";
booleanoString(false) -> "false".

mapa_para_string({Username, {Pass, Estado, Vitorias, Derrotas, Nivel}}) ->
    "{" ++ "\"" ++ Username ++ "\"" ++ ", {" ++ "\"" ++ Pass ++ "\"" ++ "," ++ 
    booleanoString(Estado) ++ "," ++ integer_to_list(Vitorias) ++ "," ++ 
    integer_to_list(Derrotas) ++ "," ++ integer_to_list(Nivel) ++ "}}".

maps_para_string([]) -> "";
maps_para_string([H]) -> mapa_para_string(H);
maps_para_string([H|T]) -> mapa_para_string(H) ++ "\n" ++ maps_para_string(T).

loop(Map) ->
    receive
        {{create_account, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                error when Username =:= "" orelse Pass =:= "" ->
                    From ! bad_arguments,
                    loop(Map);
                error ->
                    From ! ok,
                    NewMap = maps:put(Username, {Pass, false, 0, 0, 1}, Map),
                    save_map(NewMap),
                    loop(NewMap);
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{close_account, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, _, _, _, _}} ->
                    From ! ok,
                    NewMap = maps:remove(Username, Map),
                    save_map(NewMap),
                    loop(NewMap);
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{login, Username, Pass}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, false, V, D, N}} ->
                    From ! ok,
                    loop(maps:put(Username, {Pass, true, V, D, N}, Map));
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{logout, Username}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, true, V, D, N}} ->
                    From ! ok,
                    loop(maps:put(Username, {Pass, false, V, D, N}, Map));
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{update_loss, Username}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, Estado, Vitorias, Derrotas, Nivel}} ->
                    NovaDerrota = Derrotas + 1,
                    NovoNivel = if 
                        Nivel == 1 -> Nivel;
                        true -> 
                            case NovaDerrota >= erlang:ceil(Nivel / 2) of
                                true -> Nivel - 1;
                                false -> Nivel
                            end
                    end,
                    NewMap = maps:put(Username, {Pass, Estado, Vitorias, NovaDerrota, NovoNivel}, Map),
                    save_map(NewMap),
                    From ! ok,
                    loop(NewMap);
                _ ->
                    From ! invalid,
                    loop(Map)
            end;

        {{update_win, Username}, From} ->
            case maps:find(Username, Map) of
                {ok, {Pass, Estado, Vitorias, Derrotas, Nivel}} ->
                    NovaVitoria = Vitorias + 1,
                    NovoNivel = if 
                        NovaVitoria >= Nivel -> Nivel + 1;
                        true -> Nivel
                    end,
                    NewMap = maps:put(Username, {Pass, Estado, NovaVitoria, Derrotas, NovoNivel}, Map),
                    save_map(NewMap),
                    From ! ok,
                    loop(NewMap);
                _ ->
                    From ! invalid,
                    loop(Map)
            end
    end.