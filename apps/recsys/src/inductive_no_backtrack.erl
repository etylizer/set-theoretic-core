-module(inductive_no_backtrack).

-compile([export_all, nowarn_unused_type, nowarn_export_all]).

-type phi() :: inductive:phi().
-type variable() :: inductive:variable().

-type long_constraint() :: variable() | {phi(), long_constraint()}.
-type table() :: #{variable() => 0 | 1 | [C_i :: long_constraint()]}. % with table(v(C_i)) not in {0, bottom}

-spec v(long_constraint()) -> variable().
v({_, C}) -> v(C);
v(Var) -> Var.

-spec domain(table()) -> sets:set(variable()).
domain(O) ->
  % maps:keys(O).
  sets:from_list(maps:keys(O)).

-spec true_variables(table()) -> sets:set(variable()).
true_variables(O) ->
  sets:from_list([Var || Var := 1 <- O]).

-spec leq(table(), table()) -> boolean().
leq(O1, O2) ->
  sets:is_subset(domain(O2), domain(O1))
  and 
  sets:is_subset(true_variables(O2), true_variables(O1)).

-spec extend(table(), variable(), todo) -> todo.
extend(O, T, S) ->
  case O of
    #{T := _} -> O; % o(t) not bottom
    _ ->
      O0 = trigger(O#{T => []}, {maps:get(T, S), T}, S),
      % if the state of t is a set of constraints [C1; . . . ; Cn], 
      % we can sometimes replaces it with 0, which will save us from extending this set later.
      % we have to be sure that t is indeed 0 first
      % that is the case if none of the other suspended constraints in the current table 
      % is "pointing" towards t
      case may(O0, T, S) of
        true -> O0;
        false -> O0#{T => 0}
      end
  end.

trigger(O, C, S) ->
  Vc = v(C),
  case O of
    % when activating the constraint C,
    % if we know that the truth value of Vc is 1,
    % we can ignore this constraint
    #{Vc := 1} -> O;
    _ -> trigger2(O, C, S)
  end.

trigger2(O, T, S) when is_atom(T) ->
  #{T := L} = O,
  trigger3(O#{T => 1}, L, S);
trigger2(O, {T, C}, S) when is_atom(T) ->
  case v(C) of
    % a constraint of the form t => ... => t can be ignored
    T -> O;
    _ ->
      O0 = extend(O, T, S),
      case O0 of
        #{T := 0} -> O0;
        #{T := 1} -> trigger(O0, C, S);
        #{T := L} -> 
          O0#{T => [C | L]}
      end
  end;
trigger2(O, {0, _C}, _S) -> O;
trigger2(O, {1, C}, S) -> trigger2(O, C, S);
trigger2(O, {{'and', F1, F2}, C}, S) -> trigger2(O, {F1, {F2, C}}, S);
trigger2(O, {{'or', F1, F2}, C}, S) -> trigger(trigger2(O, {F1, C}, S), {F2, C}, S).

trigger3(O, [], _S) -> O;
trigger3(O, [C | L], S) -> trigger3(trigger(O, C, S), L, S).


% assumption: T is in O
may(O, T, _S) ->
  case O of
    #{T := 1} -> true;
    _ ->
      length([ok || _Tp := V <- O, is_list(V), lists:any(fun(C) -> case v(C) of T -> true; _ -> false end end, V) ]) > 0
  end.

clean(O) -> #{K => case V of 0 -> 0; 1 -> 1; _ -> 0 end || K := V <- O}.

 
-spec 'Example 7.16_test'() -> ok.
'Example 7.16_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => t1
  },
  #{t1 := 1, t2 := 1} = extend(#{}, t1, S),
  ok.

-spec 'Example 7.17_test'() -> ok.
'Example 7.17_test'() ->
  S = #{
    t1 => {'or', t2, 1},
    t2 => 0
  },
  % t2 := 0 is now in the negative cache
  #{t1 := 1, t2 := 0} = extend(#{}, t1, S),
  ok.

-spec 'Example 7.25_test'() -> ok.
'Example 7.25_test'() ->
  S = #{
    t1 => t2,
    t2 => t1
  },
  #{t1 := 0, t2 := 0} = clean(extend(#{}, t1, S)),
  ok.

cache_test() ->
  S = #{
    t1 => {'and', t2, {'and', t2, 1}},
    t2 => 1
  },
  % cache computes t2 only once
  #{t1 := 1, t2 := 1} = extend(#{}, t1, S).

-spec shortcut_test() -> ok.
shortcut_test() ->
  S = #{
    t1 => {'or', 1, t2},
    t2 => t1
  },
  % t2 is not explored
  #{t1 := 1} = extend(#{}, t1, S),
  ok.
