-module(funfunc).
-export([fib/1]).
-export([factorial/1]).

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

factorial(1) -> 1;
factorial(N) -> N * factorial(N - 1).
