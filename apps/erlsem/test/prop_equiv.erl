-module(prop_equiv).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([limited_formula/0]).

tany() -> {predef, any}.
tempty() -> {predef, none}.
tunion(A, B) -> {union, [A, B]}.
tintersection(A, B) -> {intersection, [A, B]}.
tarrow(A, B) -> {fun_full, [A], B}.
tvar() ->
  ?LET(Varname, oneof(types()), {named, 0, {ty_ref, '.', Varname, 0}, []}).


types() -> [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10].

limited_formula() ->
  ?SIZED(Size, limited_formula(Size, toplevel)).

tvar_if_not_toplevel(Mode) -> 
  case Mode of toplevel -> []; _ -> [{1, tvar()}] end.


limited_formula(Size, Mode) when Size =< 1 ->
  frequency([
    {1, tempty()},
    {1, tany()}
  ] ++ tvar_if_not_toplevel(Mode)
);
limited_formula(Size, Mode) ->
  frequency([
    {1, tempty()},
    {1, tany()},
    {2, ?LAZY(?LET({A, B}, 
        {limited_formula(Size div 2, Mode), limited_formula(Size div 2, Mode)}, 
        tunion(A, B)))  },
    {2, ?LAZY(?LET({A, B}, 
        {limited_formula(Size div 2, Mode), limited_formula(Size div 2, Mode)}, 
        tintersection(A, B)))  },
    {2, ?LAZY(?LET({A, B}, 
        {limited_formula(Size div 2, inside), limited_formula(Size div 2, inside)}, 
        tarrow(A, B)))  }
  ] ++ tvar_if_not_toplevel(Mode)
).

system(Variables) ->
  ?SUCHTHAT(Ty, ?LET(Formulas, [limited_formula() || _ <- Variables], 
    maps:from_list(lists:zip(Variables, Formulas))
  ), valid_system(Ty)).


prop_single() ->
  System = 
  #{
    t1 => {union,[
      {fun_full,[{predef,any}],{predef,any}},
      {fun_full,[{predef,none}],{predef,any}}
    ]}
  },
  

  global_state:with_new_state(fun() ->
    maps:foreach(fun(VarName, AstTy) ->
      ty_parser:extend_symtab(VarName, {ty_scheme, [], AstTy})
    end, System),

    maps:map(fun(Name, _) -> 
      io:format(user, "Parsing ~p~n", [Name]),
      Ty = {named, noloc, {ty_ref, '.', Name, 0}, []},
      % parse
      Node = ty_parser:parse(Ty),
      ty_node:is_empty(Node),
      ty_node:is_empty(Node),
      true
    end, System),
    
    true
  end),
  true.

% property that checks if we can parse any random type
prop_parse_and_emptiness() -> 
  ?FORALL(X, system(types()), begin 
    global_state:with_new_state(fun() ->
      maps:foreach(fun(VarName, AstTy) ->
        ty_parser:extend_symtab(VarName, {ty_scheme, [], AstTy})
      end, X),

      % io:format(user, "Set a new system of equations...~n", []),
      % io:format(user, "~p~n", [ty_parser:get_symtab()]), 
      
      maps:map(fun(Name, _) -> 
        Ty = {named, noloc, {ty_ref, '.', Name, 0}, []},
        % parse
        Parsed = ty_parser:parse(Ty),
        ty_node:is_empty(Parsed),
        true
        % ty_node:is_empty()
      end, X),
      true 
    end)
  end).



valid_system(System) ->
  lists:all(fun valid_rec/1, maps:to_list(System)).
  

valid_rec({_, {predef, any}}) -> true;
valid_rec({_, {predef, none}}) -> true;
valid_rec({Ty, {union, L}}) -> lists:all(fun(E) -> valid_rec({Ty, E}) end, L);
valid_rec({Ty, {intersection, L}}) -> lists:all(fun(E) -> valid_rec({Ty, E}) end, L);
valid_rec({_, {fun_full, _, _}}) -> true;
valid_rec({Ty, {named, _, {ty_ref, '.', Ty, 0}, []}}) -> false;
valid_rec({_, {named, _, _Ty, []}}) -> false. % lets say recursion happens only under a type constructor for any variable
  