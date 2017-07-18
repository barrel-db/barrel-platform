-file("eqc-1.41.2/examples/queue_model.erl", 0).
%% This module defines the behaviour of the queue library, by
%% modelling queues as lists. It is used by queue_eqc to check the
%% results that the queue library returns.

-module(queue_model).
-compile(export_all).

new() ->
    [].

from_list(L) ->
    L.

in(X,L) ->
    L++[X].

in_r(X,L) when is_list(L) ->
    [X|L].

out([]) ->
    [];
out(L) ->
    tl(L).

out_r([]) ->
    [];
out_r(L) ->
    lists:reverse(tl(lists:reverse(L))).
%    tl(L).

reverse(L) ->
    lists:reverse(L).

split(I,N,L) ->
    element(I,lists:split(N,L)).

cons(X,L) when is_list(L) ->
    [X|L].

snoc(L,X) ->
%    cons(X,L).
    L ++ [X].

drop(L) ->
    tl(L).

tail(L) ->
    tl(L).

drop_r(L) ->
%    drop(L).
    lists:reverse(tl(lists:reverse(L))).

init(L) ->
    drop_r(L).

join(L1,L2) when is_list(L2) ->
    L1 ++ L2.
%    L2++L1.

is_queue(L) ->
    is_list(L).

is_empty(L) when is_list(L) ->
    L == [].

to_list(L) when is_list(L) ->
    L.

len(L) ->
    length(L).

head(L) ->
    hd(L).

last(L) ->
    lists:last(L).

peek([]) ->
    empty;
peek(L) ->
    {value,hd(L)}.

peek_r([]) ->
    empty;
peek_r(L) ->
    {value,lists:last(L)}.

get(L) ->
    hd(L).

get_r(L) ->
    lists:last(L).

member(X,L) ->
    lists:member(X,L).

filter(L) ->
    [X || X <- L,
	  X rem 2==0].

