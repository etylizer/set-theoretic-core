-module(ty_parser).

-compile([export_all, nowarn_export_all]).

% global state
-spec init() -> _.
init() ->
  case ets:whereis(?MODULE) of
      undefined -> 
        ets:new(?MODULE, [set, named_table, {keypos, 1}]),
        ets:insert(?MODULE, {state, #{symtab => #{}, unify => #{}, cache => #{}, ref_to_ty => #{}, ty_to_ref => #{} }});
      _ -> 
        ok % cleanup()
  end,
  % io:format(user, "ty_node state initialized~n", []).
  ok.

-spec clean() -> _.
clean() ->
  case ets:whereis(?MODULE) of
      undefined -> ok;
      _ -> 
        % io:format(user, "ty_node state removed~n", []),
        ets:delete(?MODULE)
  end.

-type temporary_ref() :: 
    {local_ref, integer()} % fresh type references created for the queue
  | {named_ref, {Ref :: term(), Args :: term()}}.

-spec new_local_ref() -> temporary_ref().
new_local_ref() -> {local_ref, erlang:unique_integer()}.

new_local_ref(Term) -> {local_ref, erlang:phash2(Term)}.


extend_symtab(Key, Value) ->
  (S = #{symtab := Sym}) = global_state:get_state(?MODULE),
  global_state:set_state(?MODULE, S#{symtab := Sym#{Key => Value}}).

get_symtab() ->
  #{symtab := Sym} = global_state:get_state(?MODULE),
  Sym.

set_symtab(Symtab) ->
  S = global_state:get_state(?MODULE),
  global_state:set_state(?MODULE, S#{symtab := Symtab}).


% -spec var_ref(ast:ty_var()) -> temporary_ref().
% var_ref(Var) -> {mu_ref, Var}.

% -spec named_ref(ast:ty_ref(), [ast:ty()]) -> temporary_ref().
named_ref(Def, Args) -> {named_ref, {Def, Args}}.

% -define(CACHE, ty_parser_local_cache).
% % local state
% reset() ->
%   put(symtab, #{}),
%   put(?CACHE, undefined),
%   gett(),
%   ok.

lookup_ty({ty_ref, _, Ref, _}) ->
  % ({ty_scheme, Vars, Ty}) = symtab:lookup_ty(Ref, Loc, Sym),
  #{symtab := #{Ref := {ty_scheme, [], Ty}}} = global_state:get_state(?MODULE),
  {ty_scheme, [], Ty}.

% -spec ast_to_erlang_ty(ast:ty(), symtab:t()) -> ty_rec:ty_ref().
parse(Ty) ->
  % io:format(user, "Parsing ~p~n", [Ty]),
  % io:format(user, "State ~p~n", [gett()]),
  (S = #{unify := U, ref_to_ty := RefToTy, ty_to_ref := TyToRef}) = global_state:get_state(?MODULE),
  % io:format(user, "Parsing ~p~nReusing parser cache:~n~p~n~p~n", [Ty, RefToTy, TyToRef]),
  % 1. Convert to temporary local representation
  % Create a temporary type equation with a first entrypoint LocalRef = ...
  % and parse the type layer by layer
  % use local type references stored in a local map
  LocalRef = new_local_ref(Ty),
  (Result = {NewR,NewT}) = convert(queue:from_list([{LocalRef, Ty}]), {RefToTy, TyToRef}),
  % io:format(user, "Result:~n~p~n", [{LocalRef, Result}]),
 
  % 2. Unify the results
  % There can be many duplicate type references;
  % these will be substituted with their representative
  case U of
    #{LocalRef := ReplacedRef} -> 
      % save cache for future
      global_state:set_state(?MODULE, S#{ref_to_ty => NewR, ty_to_ref => NewT}),
      
      % io:format(user,"Unify cache hit~n", []),
      ReplacedRef;
    _ ->
      % really unify
      {UnifiedRef, UnifiedResult} = unify(LocalRef, Result),
      % {M1, _} = Result,
      % {UnifiedRef, UnifiedResult} = {LocalRef, M1},
      % io:format(user, "Unified Result:~n~p~n", [{UnifiedRef, UnifiedResult}]),
      % io:format(user,"Parsed:~n~p~n~p~n", [UnifiedRef, UnifiedResult]),

      % 3. create new type references and replace temporary ones
      %    return result reference
      ReplaceRefs = maps:from_list([{Ref, ty_node:new_ty_node()} || Ref <- maps:keys(UnifiedResult)]),
      {ReplacedRef, ReplacedResults} = replace_all({UnifiedRef, UnifiedResult}, ReplaceRefs),
      % io:format(user,"Replaced:~n~p~n~p~n", [ReplacedRef, ReplacedResults]),

      % 4. define types
      [ty_node:define(Ref, ToDefineTy) || Ref := ToDefineTy <- ReplacedResults],

      % save unify cache
      global_state:set_state(?MODULE, S#{unify => U#{LocalRef => ReplacedRef}, ref_to_ty => NewR, ty_to_ref => NewT}),
      
      ReplacedRef
  end.

replace_all({Ref, All}, Map) ->
  utils:everywhere(fun
    (RRef = {X, _}) when X == local_ref; X == mu_ref; X == named_ref -> 
      case Map of
        #{RRef := Replace} -> {ok, Replace};
        _ -> error
      end;
    (_) -> error
  end, {Ref, All}).
  

% -type queue() :: queue:queue({temporary_ref(), ast:ty(), memo()}).
% -type memo() :: #{}.
% -type ty_rec() :: ty_rec:ty_rec().
% local database of type references; 
% -type result() :: {#{temporary_ref() => ty_rec()}, #{ty_rec() => [temporary_ref()]}}. 


% -spec group(#{A => list(X)}, A, X) -> #{A := list(X)}.
group(M, Key, Value) ->
  case M of
    #{Key := Group} -> M#{Key => lists:usort(Group ++ [Value])};
    _ -> M#{Key => [Value]}
  end.

% -spec convert(queue(), symtab:t(), result()) -> result().
convert(Queue, Res) ->
  case queue:is_empty(Queue) of
    true -> 
      Res; 
    _ -> % convert next layer
      {{value, {LocalRef, Ty}}, Q} = queue:out(Queue),
      {ErlangRecOrLocalRef, NewQ, {R1, R2}} = do_convert({Ty, Res}, Q),
      convert(NewQ, {R1#{LocalRef => ErlangRecOrLocalRef}, group(R2, ErlangRecOrLocalRef, LocalRef)})
  end.

% -spec do_convert({ast:ty(), result()}, queue(), symtab:t(), memo()) -> {ty_rec(), queue(), result()}.

% entrypoint for recursion
% named
do_convert({{named, _, Ref, Args}, R = {IdTy, _}}, Q) ->
  (S = #{cache := M}) = global_state:get_state(?MODULE),
  case M of
    #{{Ref, Args} := NewRef} ->
      #{NewRef := Ty} = IdTy,
      % io:format(user, "Cache hit for parse: ~p~n~p -> ~p~n", [Ref, NewRef, Ty]),
      {Ty, Q, R};
    _ ->
      % find ty in global table
      % io:format(user,"Lookup: ~p~n", [Ref]),
      ({ty_scheme, [], Ty}) = lookup_ty(Ref),
      % TODO apply args to ty scheme
      % Map = subst:from_list(lists:zip([V || {V, _Bound} <- Vars], Args)),
      % NewTy = subst:apply(Map, Ty, no_clean),
      NewTy = Ty,
      
      % create a new reference (ref args pair) and memoize
      NewRef = named_ref(Ref, Args),

      % new cache
      global_state:set_state(?MODULE, S#{cache => M#{{Ref, Args} => NewRef}}),
      

      {InternalTy, NewQ, {R0, R1}} = do_convert({NewTy, R}, Q),
      
      {InternalTy, NewQ, {R0#{NewRef => InternalTy}, group(R1, InternalTy, NewRef)}}
  end;
 
% built-ins
do_convert({{predef, any}, R}, Q) -> {ty_rec:any(), Q, R};
do_convert({{predef, none}, R}, Q) -> {ty_rec:empty(), Q, R};

% boolean operators
do_convert({{union, []}, R}, Q) -> {ty_rec:empty(), Q, R};
do_convert({{union, [A]}, R}, Q) -> do_convert({A, R}, Q);
do_convert({{union, [A|T]}, R}, Q) -> 
  {R1, Q1, RR1} = do_convert({A, R}, Q),
  {R2, Q2, RR2} = do_convert({{union, T}, RR1}, Q1),
  {ty_rec:union(R1, R2), Q2, RR2};

do_convert({{intersection, []}, R}, Q) -> {ty_rec:any(), Q, R};
do_convert({{intersection, [A]}, R}, Q) -> do_convert({A, R}, Q);
do_convert({{intersection, [A|T]}, R}, Q) -> 
  {R1, Q1, RR0} = do_convert({A, R}, Q),
  {R2, Q2, RR1} = do_convert({{intersection, T}, RR0}, Q1),
  {ty_rec:intersect(R1, R2), Q2, RR1};

do_convert({{negation, Ty}, R}, Q) -> 
  {NewR, Q0, RR0} = do_convert({Ty, R}, Q),
  {ty_rec:negate(NewR), Q0, RR0};

% functions
do_convert({{fun_full, Comps, Result}, R}, Q) ->
    {ETy, Q0} = lists:foldl(
        fun(Element, {Components, OldQ}) ->
            % to be converted later, add to queue
            Id = new_local_ref(),
            {Components ++ [Id], queue:in({Id, Element}, OldQ)}
        end, {[], Q}, Comps),

    % add fun result to queue
    Id = new_local_ref(),
    Q1 = queue:in({Id, Result}, Q0),
    
    T = ty_functions:singleton(length(Comps), dnf_ty_function:singleton(ty_function:function(ETy, Id))),
    {ty_rec:functions(T), Q1, R};
 
% TODO atoms
% do_convert({{singleton, Atom}, R}, Q) when is_atom(Atom) ->
%   TyAtom = ty_atom:finite([Atom]),
%   TAtom = dnf_var_ty_atom:ty_atom(TyAtom),
%   {ty_rec:s_atom(TAtom), Q, R};

% % var
% do_convert({V = {var, A}, R = {IdTy, _}}, Q) ->
%   error(todovar);
%   % % FIXME overloading of mu variables and normal variables
%   % case M of
%   %   #{V := Ref} -> % mu variable
%   %     % io:format(user,"R: ~p~n", [{Ref, R}]),
%   %     #{Ref := Ty} = IdTy,
%   %     % We are allowed to load the memoized ref
%   %     % because the second occurrence of the mu variable
%   %     % is below a type constructor, 
%   %     % i.e. the memoized reference is fully (partially) defined
%   %     {Ty, Q, R};
%   %   _ -> 
%   %     % if this is a special $mu_integer()_name() variable, convert to that representation
%   %     case string:prefix(atom_to_list(A), "$mu_") of 
%   %       nomatch -> 
%   %         {ty_rec:s_variable(ty_variable:new_with_name(A)), Q, R};
%   %       IdName -> 
%   %         % assumption: erlang types generates variables only in this pattern: $mu_integer()_name()
%   %         [Id, Name] = string:split(IdName, "_"),
%   %         {ty_rec:s_variable(ty_variable:new_with_name_and_id(list_to_integer(Id), list_to_atom(Name))), Q, R}
%   %     end
%   % end;

do_convert(T, _Q) ->
  erlang:error({"Transformation from ast:ty() to ty_rec:ty() not implemented or malformed type", T}).


% -spec unify(temporary_ref(), result()) -> {temporary_ref(), #{temporary_ref() => ty_rec()}}.
unify(Ref, {IdToTy, TyToIds}) ->
  % map with references to unify, pick representatives
  %ToUnify = maps:to_list(#{K => choose_representative(V) || K := V <- TyToIds, length(V) > 1}), 
  ToUnify = maps:to_list(maps:filtermap(fun(_K, V) when length(V) =< 1 -> false;(_K, V) -> {true, choose_representative(V)} end, TyToIds)),
  % replace equivalent refs with representative
  {UnifiedRef, {UnifiedIdToTy, _UnifiedTyToIds}} = unify(Ref, {IdToTy, TyToIds}, ToUnify),
  {UnifiedRef, UnifiedIdToTy}.

% -spec choose_representative([temporary_ref()]) -> {temporary_ref(), [temporary_ref()]}.
choose_representative(Refs) ->
  [Representative | Others] = lists:usort(
    fun
      ({Other, _}, {named_ref, _}) when Other == local_ref; Other == mu_ref -> false;
      ({local_ref, _}, {mu_ref, _}) -> false;
      ({_, X}, {_, Y}) -> X =< Y
    end, 
    Refs),
  {Representative, Others}.

% -spec unify(temporary_ref(), result(), Worklist) -> 
%   {temporary_ref(), result(), Worklist} 
%   when Worklist :: [{ty_rec(), {temporary_ref(), [temporary_ref()]}}].
% TODO utils:everywhere too slow, more efficient unify
unify(Ref, {IdToTy, TyToIds}, [{_Ty, {Representative, Duplicates}} | Xs])->
  {NewRef, {NewIdToTy, NewTyToIds}} =
  utils:everywhere(fun
    (RRef = {X, _}) when X == local_ref; X == mu_ref; X == named_ref -> 
      case lists:member(RRef, Duplicates) of
        true -> {ok, Representative};
        false -> error
      end;
    (_) -> error
  end, {Ref, {IdToTy, TyToIds}}),
  unify(NewRef, {NewIdToTy, NewTyToIds}, Xs);
unify(Ref, Db, [])->
  {Ref, Db}.