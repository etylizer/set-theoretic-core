-module(prop_equiv).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([limited_formula/0]).

vars() -> [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10].

limited_formula() ->
  ?SIZED(Size, limited_formula(Size)).

limited_formula(Size) when Size =< 1 ->
  frequency([
    {1, 0},
    {1, 1},
    {1, oneof(vars())}
  ]);
limited_formula(Size) ->
  frequency([
    {1, 0},
    {1, 1},
    {1, oneof(vars())},
    {2, ?LAZY(?LET({A, B}, 
        {limited_formula(Size - 1), limited_formula(Size - 1)}, 
        {'and', A, B}))  },
    {2, ?LAZY(?LET({A, B}, 
        {limited_formula(Size - 1), limited_formula(Size - 1)}, 
        {'or', A, B}))  }
  ]).

system(Variables) ->
  ?LET(Formulas, [limited_formula() || _ <- Variables], 
    maps:from_list(lists:zip(Variables, Formulas))
  ).

% prop_print() -> 
%     ?FORALL( X, system(), begin io:format(user, "~p~n", [X]), true end).

prop_equiv() -> 
  Vars = vars(),
  ?FORALL(X, system(Vars), 
  begin 
    lists:all(fun(Var) ->
      
      {T, Result} = timer:tc(fun() -> inductive_basic:sat(Var, X, #{}) end),
      {T2, {Result, _, _}} = timer:tc(fun() -> inductive_cache:eval(Var, X, #{}, #{}) end),
      {T3, #{Var := Result}} = timer:tc(fun() -> inductive_no_backtrack:clean(inductive_no_backtrack:extend(#{}, Var, X)) end),

      case {T, T2, T3} of
        {0,0,0} -> ok;
        {Z, _, Z} when X > 2000 -> io:format(user,"Slow!~n~p~n", [{Var, X}]);
        _ -> 
          io:format(user,"~p vs ~p vs ~p~n", [T, T2, T3])
      end,
      

      true
      % % v2
      % inductive_basic:sat(Var, X, #{}) =:= inductive_basic:sat_neg(Var, X, #{})
    end, Vars)
  end).
