-module(day1).
-export([words/1, count_to_ten/0, log/1]).

% Handles the edge case when you have two spaces.
% Also handles when you start with a space.

words([32|Rest]) -> words(Rest);
words(Sentence) -> 1 + count(Sentence).

count([]) -> 0;
count([32, 32|Rest]) -> count([32|Rest]);
count([32, Char|Rest]) -> 1 + count([Char|Rest]);
count([_|Rest]) -> count(Rest).

% counts to 10!

count_to_ten() -> ten(1).

ten(11) -> done;
ten(Num) ->
    io:format(integer_to_list(Num) ++ "~n"),
    ten(Num + 1).

% prints error sometimes

log(success) -> io:format("success~n");
log({error, Message}) -> io:format("error: ~s~n", [Message]).
