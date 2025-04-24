-module(inductive_cache).

-compile([export_all, nowarn_unused_type, nowarn_export_all]).

-type phi() :: inductive:phi().
-type variable() :: inductive:variable().
-type system() :: inductive:system().
% result contains a positive and negative cache now
% negative cache is only an approximation
-type result() :: {0 | 1, 'N'(), 'P'()}. 
-type 'N'() :: #{variable() => 0}.
-type 'P'() :: #{variable() => 1}.

-spec eval(phi(), system(), 'N'(), 'P'()) -> result().
eval(0, _S, N, P) -> {0, N, P};
eval(1, _S, N, P) -> {1, N, P};
eval({'or', Phi1, Phi2}, S, N, P) ->
  case eval(Phi1, S, N, P) of
    {1, Np, Pp} -> {1, Np, Pp};
    {0, Np, Pp} -> eval(Phi2, S, Np, Pp)
  end;
eval({'and', Phi1, Phi2}, S, N, P) ->
  case eval(Phi1, S, N, P) of
    {0, Np, Pp} -> {0, Np, Pp};
    {1, Np, Pp} -> eval(Phi2, S, Np, Pp)
  end;
eval(Variable, S, N, P) ->
  case S of #{Variable := Phi} ->
    case P of
      #{Variable := 1} -> {1, N, P};
      _ -> 
        case N of
          #{Variable := 0} -> {0, N, P};
          _ -> 
            case eval(Phi, S, N#{Variable => 0}, P) of
              {0, Nn, Pp} -> {0, Nn, Pp};
              {1, _Nn, Pp} -> {1, N, Pp#{Variable => 1}} % invalidate formulas added to N (possibly many)
            end
        end
    end
  end.

-spec 'Example 7.16_test'() -> ok.
'Example 7.16_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => t1
  },
  {1, #{}, #{t1 := 1}} = eval(t1, S, #{}, #{}),
  ok.

-spec 'Example 7.17_test'() -> ok.
'Example 7.17_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => 0
  },
  % cache misses that t2 is 0 and does not depend on t1 during computation, false dependency is added
  % we would like t2 := 0 to be in the N result cache
  {1, #{}, #{t1 := 1}} = eval(t1, S, #{}, #{}),
  ok.

-spec 'Example 7.25_test'() -> ok.
'Example 7.25_test'() ->
  S = #{
    t1 => t2,
    t2 => t1
  }, 
  % both t1 and t2 are empty, but we can't cache it
  {0, #{}, #{}} = eval(t1, S, #{}, #{}),
  ok.

cache_test() ->
  S = #{
    t1 => {'and', t2, {'and', t2, 1}},
    t2 => 1
  },
  % cache computes t2 only once
  {1, #{}, #{t1 := 1, t2 := 1}} = eval(t1, S, #{}, #{}).
