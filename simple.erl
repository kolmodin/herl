-module(simple).

-export([tok/0, foo/1, bar/0, tre3/0, tre/1, func_with_string/0, apa/1, simple_fun/0, fun_with_fun/0, fun_with_fun2/1, baz/5, baz3/0]).

tok() -> 700.

bar() -> 42.

foo(N) -> N + 1.

baz(A,B,C,D,E) -> A + B + C + D + E.

baz3() -> baz(3, 3, 3, 3 ,3).

tre3() -> tre(3).

tre(N) -> foo(N) * foo(N) * foo(N).

func_with_string() -> "hello_world_strings".

apa(N) ->
  if N > 100 -> ok100;
     N > 4 -> ok4;
     N < 2 -> ok2;
     true -> ok_true
  end.

simple_fun() ->
  A = fun(Arg) -> Arg + 42 end,
  A(8).

fun_with_fun2(A) ->
  B = fun(C) -> 5 + C() + A() end,
  3 + B(A).

fun_with_fun() ->
  fun_with_fun2(fun() -> 42 end).
