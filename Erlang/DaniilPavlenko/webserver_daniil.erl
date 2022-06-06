-module(webserver_daniil).
-export([start/0, start/1]).

start() -> start(27015).
start(PortNumber) -> 
    {ok, Listen} = gen_tcp:listen(PortNumber,[binary,{reuseaddr, true},{active, true}]),
    spawn(fun() -> listen_process(Listen) end).

listen_process(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> listen_process(Listen) end),
    request_loop(Socket, []).

request_loop(Socket, BinAcc) ->
    receive
        {tcp, Socket, Bin} ->
            request_loop(Socket, [Bin|BinAcc]);
        {tcp_closed, Socket} -> 
            Reply = process_request(list_to_binary(lists:reverse(BinAcc))),
            gen_tcp:send(Socket,Reply)
    end.

process_request(Bin) -> 
    {<<FloatDim:32/float-little>>,Matrixes} = split_binary(Bin,4),
    Dim = round(FloatDim),
    BytesInMatrix = Dim*Dim*4,
    {BinA,BinB} = split_binary(Matrixes,BytesInMatrix),
    A = binary_float32_to_matrix(BinA, Dim),
    TB = binary_float32_to_matrix_transposed(BinB,Dim),
    matrix_to_binary_float32(multiply_matrixes_tr(A,TB)).

binary_float32_to_matrix(Bin, Dim) -> 
    BytesInRow = Dim*4,
    [[ FloatVal || <<FloatVal:32/float-little>> <= BinRow] || <<BinRow:BytesInRow/binary>> <= Bin].

binary_float32_to_matrix_transposed(Bin,Dim) ->
    [gather_column(Bin,ColNum,Dim) || ColNum <- lists:seq(0,Dim-1)].

gather_column(Bin,ColNum,Dim) ->
    OffsetStart = ColNum*4,
    OffsetEnd = Dim*4-(ColNum+1)*4,
    [Float || <<_:OffsetStart/binary,Float:32/float-little,_:OffsetEnd/binary>> <= Bin].

matrix_to_binary_float32(M) -> 
    << <<Float:32/float-little>> || Row <- M, Float <- Row >>.

multiply_matrixes_tr(A,TB) -> 
    [[lists:sum(lists:zipwith(fun(X,Y)->X*Y end,Rb,Ra)) || Rb <- TB] || Ra <- A].