-module(conversores).
-export([formatState/1, formataTecla/1]).

formataTecla( Data ) ->
    Key = re:replace(Data, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    Key.

jogador_para_string(Jogador) ->
    {{{X, Y}, _, Color},U} = Jogador,
    Lista = [U,float_to_list(X, [{decimals, 3}]), float_to_list(Y, [{decimals, 3}]),integer_to_list(Color)],
    string:join(Lista, " ").


jogadores_para_string([]) -> "";
jogadores_para_string([H]) -> jogador_para_string(H) ++ " ";
jogadores_para_string([H|T]) -> jogador_para_string(H) ++ " " ++  jogadores_para_string(T).

modificador_para_string(Modifeir) ->
    {{X, Y}, Radius, _, {R, G, B}} = Modifeir,
    Lista = [float_to_list(X, [{decimals, 3}]), float_to_list(Y, [{decimals, 3}]),float_to_list(Radius, [{decimals, 3}]),integer_to_list(R),integer_to_list(G),integer_to_list(B)],
    string:join(Lista, " ").


modificadores_para_string([]) -> "";
modificadores_para_string([H]) -> modificador_para_string(H) ++ " ";
modificadores_para_string([H|T]) -> modificador_para_string(H) ++ " " ++ modificadores_para_string(T).

projectile_para_string(Bullet) ->
    {{X, Y},_, Radius, _} = Bullet,
    Lista = [float_to_list(X, [{decimals, 3}]), float_to_list(Y, [{decimals, 3}]),float_to_list(Radius, [{decimals, 3}])],
    string:join(Lista, " ").


projectiles_para_string([]) -> "";
projectiles_para_string([H]) -> projectile_para_string(H) ++ " ";
projectiles_para_string([H|T]) -> projectile_para_string(H) ++ " " ++ projectiles_para_string(T).

formatState (Estado) ->
    {ListaJogadores, ListaModificadores, ListaBalas, _} = Estado,
    Len1 = integer_to_list(length(ListaJogadores)) ++ " ",
    L1 = [{J,U} || {J, {U,_}} <- ListaJogadores ],
    R1 = jogadores_para_string(L1),
    Len2 = integer_to_list(length(ListaModificadores)) ++ " ",
    R2 = modificadores_para_string(ListaModificadores),
    Len3 = integer_to_list(length(ListaBalas)) ++ " ",
    R3 = projectiles_para_string(ListaBalas),
    Resultado = "Estado " ++ Len1 ++ R1 ++ Len2 ++ R2 ++ Len3 ++ R3 ++"\n",
    Resultado.