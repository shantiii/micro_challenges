-module(fibo).
%% O(log(n)) solution to the Fibonacci sequence

-export([fibonacci/1,fibonacci2/1]).

fibonacci(N) ->
  Memo = #{0 => 0, 1 => 1},
  {Fn, _} = do_fibo(N, Memo),
  Fn.

do_fibo(N, Memo) ->
  case maps:find(N, Memo) of
    {ok, Fn} -> {Fn, Memo};
    error ->
      case N rem 2 of
        1 ->
          {Fnp1, Memo1} = do_fibo(N div 2 + 1, Memo),
          {Fn, Memo2} = do_fibo(N div 2, Memo1),
          Ret = Fn*Fn + Fnp1*Fnp1,
          {Ret, maps:put(N, Ret, Memo2)};
        0 ->
          {Fn, Memo1} = do_fibo(N div 2, Memo),
          {Fnm1, Memo2} = do_fibo(N div 2 - 1, Memo1),
          Ret = (2*Fnm1 + Fn) * Fn,
          {Ret, maps:put(N, Ret, Memo2)}
      end
  end.

fibonacci2(N) ->
  Memo = ets:new(fibo2, [set, private]),
  ets:insert_new(Memo, {0,0}),
  ets:insert_new(Memo, {1,1}),
  Ret = do_fibo2(N, Memo),
  ets:delete(Memo),
  Ret.

do_fibo2(N, Memo) ->
  case ets:lookup(Memo, N) of
    [{N, Ret}] -> Ret;
    [] ->
      case N rem 2 of
        1 ->
          Fnp1 = do_fibo2(N div 2 + 1, Memo),
          Fn = do_fibo2(N div 2, Memo),
          Ret = Fn*Fn + Fnp1*Fnp1,
          ets:insert(Memo, {N, Ret}),
          Ret;
        0 ->
          Fn = do_fibo2(N div 2, Memo),
          Fnm1 = do_fibo2(N div 2 - 1, Memo),
          Ret = (2 * Fnm1 + Fn) * Fn,
          ets:insert(Memo, {N, Ret}),
          Ret
      end
  end.


