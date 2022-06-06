-module(matrix_connection_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    connect_test/1
]).

connect_test(Size) ->
    {ok, Port} = application:get_env(matrix, port),
    M1 = matrix_lib:gen(Size),
    M2 = matrix_lib:gen(Size),
    Expected = matrix_lib:mult(M1, M2, Size),
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}, {packet, raw}]),
    ok = gen_tcp:send(Socket, <<Size:32/little-unsigned-integer>>),
    ok = gen_tcp:send(Socket, M1),
    ok = gen_tcp:send(Socket, M2),
    {ok, Answer} = gen_tcp:recv(Socket, Size * Size * 4, 30000),
    gen_tcp:shutdown(Socket, read_write),
    gen_tcp:close(Socket),
    ?assertEqual(Expected, Answer).
