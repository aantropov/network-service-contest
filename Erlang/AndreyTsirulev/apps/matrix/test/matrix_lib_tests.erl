%% @author: Andrey
%% @date: 02.06.2022

-module(matrix_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    mult_tc/1,
    transpose_tc/1,
    mult_chunk_tc/1,
    mult_tc2/2
]).

mult_tc(Size) ->
    M1 = matrix_lib:gen(Size),
    M2 = matrix_lib:gen(Size),
    timer:tc(fun() -> matrix_lib:mult(M1, M2, Size) end).

mult_chunk_tc(Size) ->
    M1 = matrix_lib:gen(Size),
    M2 = matrix_lib:gen(Size),
    timer:tc(fun() -> matrix_lib:mult_chunk(M1, M2, Size, 0, Size * Size) end).

mult_tc2(Size, ChunkSize) ->
    M1 = matrix_lib:gen(Size),
    M2 = matrix_lib:gen(Size),
    timer:tc(fun() -> 
        Reqs = matrix_worker:send_requests(M1, M2, Size, ChunkSize),
        [ matrix_worker:receive_response(ReqId) || ReqId <- Reqs ],
        ok 
    end).

transpose_tc(Size) ->
    M1 = matrix_lib:gen(Size),
    timer:tc(fun() -> matrix_lib:transpose(M1, Size) end).

dot_test() ->
    V1 = matrix_lib:from_list([2.0, 4.0, 6.0]),
    V2 = matrix_lib:from_list([1.0, 2.0, 3.0]),
    ?assertEqual(2.0 * 1.0 + 4.0 * 2.0 + 6.0 * 3.0, matrix_lib:dot(V1, V2)).

transpose_test() ->
    M1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    M2 = [1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0],
    ?assertEqual(M2, matrix_lib:to_list(matrix_lib:transpose(matrix_lib:from_list(M1), 3))).

mult_test() ->
    M1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    M2 = lists:reverse(M1),
    Result = [30.0, 24.0, 18.0, 84.0, 69.0, 54.0, 138.0, 114.0, 90.0],
    ?assertEqual(Result, matrix_lib:to_list(matrix_lib:mult(matrix_lib:from_list(M1), matrix_lib:from_list(M2), 3))).

mult_chunk_test() ->
    M1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    M2 = lists:reverse(M1),
    Result = [30.0, 24.0, 18.0, 84.0, 69.0, 54.0, 138.0, 114.0, 90.0],
    M1Bin = matrix_lib:from_list(M1),
    M2TBin = matrix_lib:transpose(matrix_lib:from_list(M2), 3),
    ?assertEqual(Result, matrix_lib:to_list(matrix_lib:mult_chunk(M1Bin, M2TBin, 3, 0, 100))),
    ?assertEqual([84.0, 69.0], matrix_lib:to_list(matrix_lib:mult_chunk(M1Bin, M2TBin, 3, 3, 2))).

worker_test() ->
    Size = 1000,
    ChunkSize = 10000,
    M1 = matrix_lib:gen(Size),
    M2 = matrix_lib:gen(Size),
    Reqs = matrix_worker:send_requests(M1, M2, Size, ChunkSize),
    M3 = iolist_to_binary([ matrix_worker:receive_response(ReqId) || ReqId <- Reqs ]),
    ?assertEqual(matrix_lib:mult(M1, M2, Size), M3).
