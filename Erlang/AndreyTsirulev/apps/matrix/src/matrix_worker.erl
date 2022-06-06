-module(matrix_worker).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
	start_link/0,
    send_request/6,
    receive_response/1,
    workers/0,
    send_requests/4
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

send_request(Pid, M1, M2T, Size, ChunkStart, ChunkSize) ->
    gen_server:send_request(Pid, {mult_chunk, M1, M2T, Size, ChunkStart, ChunkSize}).

send_requests(M1, M2, Size, ChunkSize) ->
    M2T = matrix_lib:transpose(M2, Size),
    Workers = list_to_tuple(pg:get_local_members(matrix_workers)),
    WorkerCount = tuple_size(Workers),
    Worker1 = erlang:unique_integer([positive]) rem WorkerCount,
    ChunkStarts = lists:seq(0, Size * Size - 1, ChunkSize),
    [ matrix_worker:send_request(element(WorkerIndex rem WorkerCount + 1, Workers), M1, M2T, Size, ChunkStart, ChunkSize) || 
        {WorkerIndex, ChunkStart} <- lists:enumerate(Worker1, ChunkStarts) ].

receive_response(ReqId) ->
    {reply, Result} = gen_server:receive_response(ReqId, infinity),
    Result.

workers() ->
    list_to_tuple(pg:get_local_members(matrix_workers)).

init([]) ->
    pg:join(matrix_workers, self()),
    {ok, []}.

handle_call({mult_chunk, M1, M2T, Size, ChunkStart, ChunkSize}, _From, State) ->
    Result = matrix_lib:mult_chunk(M1, M2T, Size, ChunkStart, ChunkSize),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info (_Info, State) ->
    {noreply, State}.

