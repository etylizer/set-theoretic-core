-module(ty_node).

-compile([export_all, nowarn_export_all]).

-behaviour(global_state).

% -record(ty_node, {id :: integer(), definition :: term()}).

compare({node, Id1}, {node, Id2}) when Id1 < Id2 -> lt;
compare({node, Id1}, {node, Id2}) when Id1 > Id2 -> gt;
compare({node, _Id1}, {node, _Id2}) -> eq;
% this is an architecture hack;
% ty_rec is used differently in ty_parser with local references
% ty_node has to support comparing these local temporary references
compare({local_ref, Id1}, {local_ref, Id2}) when Id1 < Id2 -> lt;
compare({local_ref, Id1}, {local_ref, Id2}) when Id1 > Id2 -> gt;
compare({local_ref, _Id1}, {local_ref, _Id2}) -> eq.


make(Ty) ->
  define(new_ty_node(), Ty).

new_ty_node() ->
  {node, next_id()}.

define(Reference, Node) ->
  (S = #{system := System}) = global_state:get_state(?MODULE),
  New = System#{Reference => Node},
  global_state:set_state(?MODULE, S#{system => New}),
  Reference.

-spec init() -> _.
init() ->
  case ets:whereis(?MODULE) of
      undefined -> 
        ets:new(?MODULE, [set, named_table, {keypos, 1}]),
        ets:insert(?MODULE, {state, #{id => 0, system => #{}, p => #{}, n => #{}, s => stack:new()}});
      _ -> 
        ok % cleanup()
  end,
  % io:format(user, "ty_node state initialized~n", []).
  ok.

next_id() ->
  (S = #{id := Id}) = global_state:get_state(?MODULE),
  global_state:set_state(?MODULE, S#{id => Id + 1}),
  Id + 1.

-spec clean() -> _.
clean() ->
  case ets:whereis(?MODULE) of
      undefined -> ok;
      _ -> 
        % io:format(user, "ty_node state removed~n", []),
        ets:delete(?MODULE)
  end.

load(TyNode) ->
  (#{system := #{TyNode := Ty}}) = global_state:get_state(?MODULE),
  Ty.
  
leq(T1, T2) ->
  is_empty(difference(T1, T2)).

% see Frisch PhD thesis
% TODO merge P and N into one ETS table
% TODO implement backtracking-free algorithm
is_empty(TyNode) ->
  (S = #{p := P, n := N, s := Stack}) = global_state:get_state(?MODULE),
  Ty = load(TyNode),

  % stack keeps track of the same items in N
  true = length(Stack) =:= length(maps:keys(N)),

  case {P, N} of
    {#{Ty := false}, _} -> 
      % io:format(user,"p", []),
      false; % cache hit
    {_, #{Ty := true}} -> 
      % io:format(user,"n", []),
      true; % cache hit
    _ -> 
      % assume type is empty
      % and add to state

      % update global cache
      % N U {t}, put t on stack
      global_state:set_state(?MODULE, S#{n => N#{Ty => true}, s => stack:push(Ty, Stack)}),
      
      Result = ty_rec:is_empty(Ty),

      case Result of 
        % empty; global cache can be kept as is (i.e. TyNode is empty is now cached)
        true -> true;

        % not empty;
        % invalidate all types that were assumed to be empty
        % we need to recover initial N via the stack
        % TODO since we are in the immutable Erlang world anyway, use the N in Line 25...
        false -> 
          backtrack(Ty),
          false
      end
  end.

backtrack(Ty) ->
  (S = #{p := P, n := N, s := Stack}) = global_state:get_state(?MODULE),
  case stack:pop(Stack) of
    {Ty, Rest} ->
      global_state:set_state(?MODULE, S#{ p => P#{Ty => false}, n => maps:remove(Ty, N), s => Rest}),
      ok;
    {OtherNode, Rest} ->
      global_state:set_state(?MODULE, S#{n => maps:remove(OtherNode, N), s => Rest}),
      backtrack(Ty);
    empty -> error(empty_stack)
  end.

negate(T) ->
  make(ty_rec:negate(load(T))).

intersect(T1, T2) ->
  make(ty_rec:intersect(load(T1), load(T2))).

union(T1, T2) ->
  make(ty_rec:union(load(T1), load(T2))).

difference(T1, T2) ->
  make(ty_rec:difference(load(T1), load(T2))).

any() ->
  make(ty_rec:any()).

empty() ->
  make(ty_rec:empty()).

disjunction(Nodes) ->
  lists:foldl(fun(E, Acc) -> union(E, Acc) end, empty(), Nodes).

conjunction(Nodes) ->
  lists:foldl(fun(E, Acc) -> intersect(E, Acc) end, any(), Nodes).