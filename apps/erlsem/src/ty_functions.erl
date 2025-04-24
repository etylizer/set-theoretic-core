-module(ty_functions).

-compile([export_all, nowarn_export_all]).

compare(Fs1, Fs2) ->
  error({todo, impl, Fs1, Fs2}).

empty() ->
  {dnf_ty_function:empty(), #{}}.

any() ->
  {dnf_ty_function:any(), #{}}.

is_empty({Default, AllFunctions}) ->
  dnf_ty_function:is_empty(Default)
    andalso
    maps:fold(fun(_Size, V, Acc) -> Acc andalso dnf_ty_function:is_empty(V) end, true, AllFunctions).

singleton(Length, FunctionDnf) when is_integer(Length) ->
  {dnf_ty_function:empty(), #{Length => FunctionDnf}}.

negate({DefaultF, F}) ->
  {dnf_ty_function:negate(DefaultF), maps:map(fun(_K,V) -> dnf_ty_function:negate(V) end, F)}.


union({DefaultF1, F1}, {DefaultF2, F2}) ->
  % get all keys
  AllKeys = maps:keys(F1) ++ maps:keys(F2),
  UnionKey = fun(Key) ->
    dnf_ty_function:union(
      maps:get(Key, F1, DefaultF1),
      maps:get(Key, F2, DefaultF2)
    )
                 end,
  {dnf_ty_function:union(DefaultF1, DefaultF2), maps:from_list([{Key, UnionKey(Key)} || Key <- AllKeys])}.

intersect({DefaultF1, F1}, {DefaultF2, F2}) ->
  % get all keys
  AllKeys = maps:keys(F1) ++ maps:keys(F2),
  IntersectKey = fun(Key) ->
    dnf_ty_function:intersect(
      maps:get(Key, F1, DefaultF1),
      maps:get(Key, F2, DefaultF2)
    )
                 end,
  {dnf_ty_function:intersect(DefaultF1, DefaultF2), maps:from_list([{Key, IntersectKey(Key)} || Key <- AllKeys])}.

difference({DefaultF1, F1}, {DefaultF2, F2}) ->
  % get all keys
  AllKeys = maps:keys(F1) ++ maps:keys(F2),
  DifferenceKey = fun(Key) ->
    dnf_ty_function:difference(
      maps:get(Key, F1, DefaultF1),
      maps:get(Key, F2, DefaultF2)
    )
                 end,
  {dnf_ty_function:difference(DefaultF1, DefaultF2), maps:from_list([{Key, DifferenceKey(Key)} || Key <- AllKeys])}.

% TODO tally
% normalize_corec({Default, AllFunctions}, Fixed, M) ->
%   Others = ?F(
%     maps:fold(fun(Size, V, Acc) ->
%       constraint_set:meet(
%         ?F(Acc),
%         ?F(dnf_var_ty_function:normalize_corec(Size, V, Fixed, M))
%       )
%               end, [[]], AllFunctions)
%   ),

%   DF = ?F(dnf_var_ty_function:normalize_corec({default, maps:keys(AllFunctions)}, Default, Fixed, M)),

%   constraint_set:meet(
%     DF,
%     Others
%   ).


% substitute({DefaultFunction, AllFunctions}, SubstituteMap, Memo) ->
%   % see multi_substitute for comments
%   % TODO refactor abstract into one function for both tuples and funcions
%   NewDefaultFunction = dnf_var_ty_function:substitute(DefaultFunction, SubstituteMap, Memo, fun(Ty) -> ty_rec:pi({function, default}, Ty) end),
%   AllVars = dnf_var_ty_function:all_variables(DefaultFunction),
%   % TODO erlang 26 map comprehensions
%   Keys = maps:fold(fun(K,V,AccIn) -> case lists:member(K, AllVars) of true -> ty_rec:function_keys(V) -- maps:keys(AllFunctions) ++ AccIn; _ -> AccIn end end, [], SubstituteMap),
%   % Keys = [function_keys(V) || K := V <- SubstituteMap, lists:member(K, AllVars)],
%   OtherFunctionKeys = lists:usort(lists:flatten(Keys)),
%   NewDefaultOtherFunctions = maps:from_list([{Length, dnf_var_ty_function:substitute(DefaultFunction, SubstituteMap, Memo, fun(Ty) -> ty_rec:pi({function, Length}, Ty) end)} || Length <- OtherFunctionKeys]),
%   AllKeys = maps:keys(AllFunctions) ++ maps:keys(NewDefaultOtherFunctions),

%   NewOtherFunctions = maps:from_list(lists:map(fun(Key) ->
%     {Key, case maps:is_key(Key, AllFunctions) of
%             true -> dnf_var_ty_function:substitute(maps:get(Key, AllFunctions), SubstituteMap, Memo, fun(Ty) -> ty_rec:pi({function, Key}, Ty) end);
%             _ -> maps:get(Key, NewDefaultOtherFunctions, NewDefaultFunction)
%           end}
%                                             end, AllKeys)),

%   {NewDefaultFunction, NewOtherFunctions}.