-module(matrix_connection).

-include_lib("kernel/include/logger.hrl").

-export([
    handle/1
]).

handle(Socket) ->
    receive
        start -> ok
    after 1000 -> exit(normal)
    end,
    {ok, <<SizeF:32/little-float>>} = gen_tcp:recv(Socket, 4, 5000),
    Size = round(SizeF),
%    ?LOG_INFO(#{what => matrix_request, size => Size}),
    {ok, M1} = gen_tcp:recv(Socket, Size * Size * 4, 30000),
    {ok, M2} = gen_tcp:recv(Socket, Size * Size * 4, 30000),
    MinSplitSize = application:get_env(matrix, min_split_size, 100),
    case Size > MinSplitSize of
        true ->
            ChunkSize = application:get_env(matrix, chunk, 1000),
            Reqs = matrix_worker:send_requests(M1, M2, Size, ChunkSize),
            Data = [ matrix_worker:receive_response(ReqId) || ReqId <- Reqs ],
            ok = gen_tcp:send(Socket, Data);
        false ->
            Result = matrix_lib:mult(M1, M2, Size),
            ok = gen_tcp:send(Socket, Result)
    end,
    prim_inet:setopts(Socket, [{active, once}]),
    gen_tcp:shutdown(Socket, write),
    receive
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, Reason} ->
            ?LOG_ERROR(#{what => matrix_tcp_connection_error, reason => Reason})
    after 30000 ->
        ?LOG_ERROR(#{what => matrix_tcp_send_timeout})
    end,
    gen_tcp:close(Socket).
