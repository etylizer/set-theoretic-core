-module(musem_hashing).

%  This module implements various methods to check 
%  different equivalence classes for structure sharing.
%  We import some definitions and 
%  implement a helper function to generate fresh recursion variables.

-import(musem, [any/0, empty/0]).

-type ast() :: musem:ast().
-type var_name() :: musem:var_name().
-type rec_var() :: musem:rec_var().

%  To define fresh variables, we need to generate unique integers.
%  We make use of the Erlang built-in unique_integer/0.
-spec fresh_rec_var() -> rec_var().
fresh_rec_var() -> {recursion_variable, erlang:unique_integer()}.

%  =================================
%  = Equivalence up to mu-renaming =
%  =================================
% 
%  Mu-equivalence means terms equivalent up to renaming of the recursion variables.
%
%  The following is an implementation of the paper
%  "Hashing Modulo Alpha-Equivalence (PLDI21)",
%  where we implement the esummary for equivalence checking 
%  of terms which are equivalent modulo mu-renaming.

%  The summary of an "ast()" term is described by the shape of the term without variables
%  and a variable map filling the shape with proper variable names.
-type esummary() :: {summary, estructure(), evarmap()}.

%  Then, the summary of an expression has the following definition.
-spec summarize(ast()) -> esummary().

%  Further, there is a method to reconstruct an ast() term from a summary, 
%  up to alpha-renaming of recursion variables. 
%  But we don't need it yet.
-spec rebuild(esummary()) -> ast().
rebuild(_) -> error(not_implemented).

%  The structure redefines the "ast()" by replacing variables with anonymous variables
%  and variable binders with position markers which describe
%  where the variable is bound in its body.
-type may(T) :: T | error.
-type estructure() :: 
  {eflag} 
| {eproduct, estructure(), estructure()} 
| {enegation, estructure()}
| {eunion, estructure(), estructure()}
| {anonymous_variable}
| {emu, may(pos_tree()), estructure()}.

%  The variable map maps names to position trees.
-type evarmap() :: #{var_name() => pos_tree()}.

% some helper functions to handle variable maps
-spec remove_from_vm(var_name(), evarmap()) -> {evarmap(), may(pos_tree())}.
remove_from_vm(Var, V_body) ->
  case maps:is_key(Var, V_body) of
    true -> {maps:remove(Var, V_body), maps:get(Var, V_body)};
    _ -> {maps:remove(Var, V_body), error}
  end.

%  TODO instead of may(pos_tree()), why not include var_none in pos_tree? Worse complexity?
%  Position trees describe the position of the variable of a binder inside the binders body.
%  It describes the same structure as the "ast()", but extends only to the point where
%  the variable last occurs.
-type pos_tree() :: 
  var_here
| {var_left, pos_tree()}
| {var_right, pos_tree()}
| {var_both, pos_tree(), pos_tree()}.

%  We can now implement the summary function.
%
%  For non-binding terms, we recursively call summarize
%  and merge the corresponding variable maps.
%  For unary terms (e.g. negation), 
%  we replace the "right-only" variable map with an empty map.
%
%  For binding terms ("mu"), we summarize the body,
%  and remove the binding variable from the resulting variable map
%  to put it into the summary structure of the binder.
summarize({flag}) -> {summary, {eflag}, #{}};
summarize({product, T1, T2}) -> 
  {summary, S1, V1} = summarize(T1),
  {summary, S2, V2} = summarize(T2),
  {summary, {eproduct, S1, S2}, merge_var_maps(V1, V2)};
summarize({negation, T}) -> 
  {summary, S, V} = summarize(T),
  {summary, {enegation, S}, merge_var_maps(V, #{})};
summarize({union, T1, T2}) -> 
  {summary, S1, V1} = summarize(T1),
  {summary, S2, V2} = summarize(T2),
  {summary, {eunion, S1, S2}, merge_var_maps(V1, V2)};
summarize({recursion_variable, Var}) -> {summary, {anonymous_variable}, #{Var => var_here}};
summarize({mu, {recursion_variable, Var}, T}) -> 
  {summary, S, V_body} = summarize(T),
  {Vm, VarPos} = remove_from_vm(Var, V_body),
  {summary, {emu, VarPos, S}, Vm}.

%  If a variable is only in the left map, then wrap the mapped value in var_left.
%  If a variable is only in the right map, then wrap the mapped value in var_right.
%  Otherwise, wrap the mapped value in var_both.
-spec merge_var_maps(evarmap(), evarmap()) -> evarmap().
merge_var_maps(VarMapLeft, VarMapRight) -> 
  M1 = #{Key => {var_left, Value} || Key := Value <- VarMapLeft, not maps:is_key(Key, VarMapRight)},
  M2 = #{Key => {var_right, Value} || Key := Value <- VarMapRight, not maps:is_key(Key, VarMapLeft)},
  M3 = #{Key => {var_both, ValueLeft, maps:get(Key, VarMapRight)} || Key := ValueLeft <- VarMapLeft, maps:is_key(Key, VarMapRight)},
  maps:merge(M1, maps:merge(M2, M3)).

-spec summary_test() -> ok.
summary_test() ->
  Var = fresh_rec_var(),
  % the variable is contained in the body
  Ty = {mu, Var, {product, {product, {flag}, Var}, Var}},
  {summary, {emu, VarPos, _}, #{}} = summarize(Ty),
  {var_both, {var_right, var_here}, var_here} = VarPos,

  % the variable is not contained in the body
  Ty2 = {mu, Var, {product, {flag}, {flag}}},
  {summary, {emu, VarPos2, _}, #{}} = summarize(Ty2),
  error = VarPos2,
  ok.

%  We can now define a predicate for alpha-equivalence testing
%  which compares the summaries of the two given types
-spec is_alpha_equivalent(ast(), ast()) -> boolean().
is_alpha_equivalent(Ty1, Ty2) -> 
  summarize(Ty1) =:= summarize(Ty2).

-spec alpha_equivalence_test() -> ok.
alpha_equivalence_test() ->
  true = is_alpha_equivalent(any(), any()),
  true = is_alpha_equivalent(empty(), empty()),
  false = is_alpha_equivalent(empty(), any()),
  ok.
