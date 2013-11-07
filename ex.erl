-module(ex).

-export([th/0, safe/0, ctch_safe/0, ctch_throw/0, ctch_case/1, try_it/1, stacker/0, stackhelper/0, failing_case/1, safely/1, safely_failing_case/0, joker2/1]).

th() ->
  throw({detta, fran, throw}).

safe() -> {'hejsan svejsan', 42}.

ctch_safe() ->
  catch safe().

ctch_throw() ->
  catch th().

ctch_case(N) ->
  catch case N of
    foo -> ok
  end.

stacker() ->
  A = 5,
  B = stacker(A),
  {A,B}.
stacker(N) ->
  St = erlang:get_stacktrace(),
  {"hello", St, N}.

stackhelper() -> {stacker(), 5}.

joker(N) ->
  if N == 1 -> throw("denna strang kastas");
     N == 2 -> erlang:exit("the exit term");
     N == 3 -> erlang:error('the error term');
     true -> {ok, N}
  end.

joker2(N) ->
  if N == 1 -> throw("denna strang kastas");
     N == 2 -> erlang:exit("the exit term");
     N == 3 -> erlang:error('the error term')
  end.

try_it(N) ->
  try joker(N) of
    A -> A
  catch
    throw:Term -> {'thrown', 'term', Term};
    exit:Reason -> {'exit', 'reason', Reason};
    error:Reason -> {'error', 'reason', Reason}
  end.

safely(F) ->
  try F() of
    R -> R
  catch
    error:Reason -> {"caught error with reason", Reason, erlang:get_stacktrace()}
  end.

safely_failing_case() -> safely(fun() -> failing_case(wazaa) end).

failing_case(N) ->
  case N of
    ok -> {ok, N};
    neok -> {neok, N}
  end.
