-module(other).

-export([remote/0, remote_foo/0, failer/0, failer2/0, fib/1, fac/1, nyling/0, fib_slow/1, tuplish/1, listish/1]).

-import(simple, [tok/0, fun_with_fun/0]).

remote() -> 42.

remote_foo() -> simple:foo(1) * 2.

failer() ->
  simple:wazaa_dont_exist().

failer2() ->
  A = fun (Arg1, 0) -> Arg1 + 1;
  	      (Arg1, _Arg2) -> Arg1 + 2 end,
  A(1).

fib(N) -> fib_helper(N,0,1).

fib_helper(Stop, A, _B) when A > Stop -> [];
fib_helper(Stop, A, B) -> [ A | fib_helper(Stop, B, A+B) ].

fib_slow(0) -> 1;
fib_slow(1) -> 1;
fib_slow(N) -> fib_slow(N-2) + fib_slow(N-1).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

nyling() ->
  Pid = erlang:spawn(other, fac, [5]),
  Pid.

tuplish(N) -> {N, N*2, N*3}.

listish(N) -> [N, N*2, N*3].