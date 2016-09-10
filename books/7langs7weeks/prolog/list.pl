% Worse because append traverses list to append
naive_reverse([], []).
naive_reverse([Head|Rest], Result) :-
    naive_reverse(Rest, Reversed),
    append(Reversed, [Head], Result).

accum(Result, [], Result).
accum(Curr, [Head|Tail], Result) :-
    accum([Head|Curr], Tail, Result).

cool_reverse(List, Result) :-
    accum([], List, Result).

smallest([], Result, Result).
smallest([Next|Rest], Curr, Result) :-
    Smallest is min(Curr, Next),
    smallest(Rest, Smallest, Result).

smallest([First|Rest], Result) :-
    smallest(Rest, First, Result).

items_in_result([], _).
items_in_result([Head|Tail], Sorted) :-
    member(Head, Sorted),
    items_in_result(Tail, Sorted).

is_sorted([Head|[Next|[]]]) :-
    Head =< Next.
is_sorted([Head|Tail]) :-
    [Next|_] = Tail,
    Head =< Next,
    is_sorted(Tail).

weird_sort(List, Result) :-
    length(List, Len),
    length(Result, Len),
    items_in_result(List, Result),
    items_in_result(Result, List),
    is_sorted(Result).
