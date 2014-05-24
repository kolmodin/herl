-module(simple).

-export([test1/0, test2/0, test3/0, inc/1, adder5/5, test_string/0, test_if/1, simple_fun/0, fun_in_fun/1, test_fun/0]).

test1() -> 700.

adder5(A,B,C,D,E) -> A + B + C + D + E.

test2() -> adder5(1, 2, 3, 4 ,5).

test3() -> tre_mult(3).

inc(N) -> N + 1.

tre_mult(N) -> inc(N) * inc(N) * inc(N).

test_string() -> "hello_world_strings".

test_if(N) ->
  if N > 100 -> ok100;
     N > 4 -> ok4;
     N < 2 -> ok2;
     true -> ok_true
  end.

simple_fun() ->
  A = fun(Arg) -> Arg + 42 end,
  A(8).

fun_in_fun(A) ->
  B = fun(C) -> 5 + C() + A() end,
  3 + B(A).

test_fun() ->
  fun_in_fun(fun() -> 42 end).
