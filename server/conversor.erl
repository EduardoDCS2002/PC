-module(conversores).
-export([formatState/1]).

formataTecla( Data ) ->
    Key = re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    Key.

% Convert player to string: "User X Y Color"
jogador_para_string({{{X, Y}, _, Color}, U}) ->
    io_lib:format("~s ~.3f ~.3f ~b", [U, X, Y, Color]).

% Convert modifier to string: "X Y Radius R G B"
modificador_para_string({{X, Y}, Radius, _, {R, G, B}}) ->
    io_lib:format("~.3f ~.3f ~.3f ~b ~b ~b", [X, Y, Radius, R, G, B]).

% Convert projectile to string: "X Y Radius"
projectile_para_string({{X, Y}, _, Radius, _}) ->
    io_lib:format("~.3f ~.3f ~.3f", [X, Y, Radius]).

% Generic list-to-string converter
list_to_string(_Converter, []) -> "";
list_to_string(Converter, List) ->
    string:join([Converter(Item) || Item <- List], " ").

% Main state formatter
formatState({Jogadores, Modificadores, Balas, _}) ->
    JogadoresStr = list_to_string(fun jogador_para_string/1, Jogadores),
    ModsStr = list_to_string(fun modificador_para_string/1, Modificadores),
    BalasStr = list_to_string(fun projectile_para_string/1, Balas),
    io_lib:format("Estado ~b ~s ~b ~s ~b ~s~n", 
                 [length(Jogadores), JogadoresStr,
                  length(Modificadores), ModsStr,
                  length(Balas), BalasStr]).