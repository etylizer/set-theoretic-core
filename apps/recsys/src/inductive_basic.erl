-module(inductive_basic).

-compile([export_all, nowarn_unused_type, nowarn_export_all]).

-type phi() :: inductive:phi().
-type variable() :: inductive:variable().
-type system() :: inductive:system().
-type result() :: 0 | 1.
-type 'N'() :: #{variable() => 0}.

-spec 
sat(phi(), system(), 'N'()) -> result().
sat(1, _S, _N) -> 1;
sat(0, _S, _N) -> 0;
sat({'or', Phi1, Phi2}, S, N) ->
  case sat(Phi1, S, N) of
    1 -> 1;
    0 -> sat(Phi2, S, N)
  end;
sat({'and', Phi1, Phi2}, S, N) ->
  case sat(Phi1, S, N) of
    0 -> 0;
    1 -> sat(Phi2, S, N)
  end;
sat(Variable, S, N) ->
  case S of #{Variable := Phi} ->
    case N of
      #{Variable := 0} -> 0;
      _ -> 
        sat(Phi, S, N#{Variable => 0})
    end
  ;_->error(bad_system)end.

-spec 'Example 7.16_test'() -> ok.
'Example 7.16_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => t1
  },
  1 = sat(t1, S, #{}).

-spec 'Example 7.17_test'() -> ok.
'Example 7.17_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => 0
  },
  1 = sat(t1, S, #{}).

-spec 'Example 7.25_test'() -> ok.
'Example 7.25_test'() ->
  S = #{
    t1 => t2,
    t2 => t1
  },
  0 = sat(t1, S, #{}),
  ok.
