-module(matrix_lib).

-export([
    gen/1,
    from_list/1,
    to_list/1,
    transpose/2,
    dot/2,
    mult/3,
    mult_chunk/5
]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(code:priv_dir(matrix), "matrix_lib"), 0).

gen(Size) ->
    Seq = lists:seq(1, Size),
    << <<(random()):32/little-float>> || _ <- Seq, _ <- Seq >>.

random() ->
    (0.5 - rand:uniform()) * rand:uniform(100000).

from_list(List) ->
    << <<I:32/little-float>> || I <- List >>.

to_list(M) ->
    [ I || <<I:32/little-float>> <= M ].

transpose(M, Size) ->
    Seq = lists:seq(0, Size - 1),
    << <<(binary:part(M, (R + C * Size) * 4, 4))/binary>> || R <- Seq, C <- Seq >>.

mult(M1, M2, Size) ->
    RowByteSize = Size * 4,
    M2T = transpose(M2, Size),
    << <<(dot(M1Row, M2Col)):32/little-float>> || <<M1Row:RowByteSize/binary>> <= M1, <<M2Col:RowByteSize/binary>> <= M2T >>.

mult_chunk(M1, M2T, Size, ChunkStart, ChunkSize) ->
    RowByteSize = Size * 4,
    Seq = lists:seq(ChunkStart, min(ChunkStart + ChunkSize, Size * Size) - 1),
    << <<(dot(row(M1, RowByteSize, I div Size), row(M2T, RowByteSize, I rem Size))):32/little-float>> || I <- Seq >>.

row(M, RowByteSize, Row) ->
    binary:part(M, Row * RowByteSize, RowByteSize).

dot(_V1, _V2) ->
    erlang:nif_error(matrix_lib_nif_not_loaded).

