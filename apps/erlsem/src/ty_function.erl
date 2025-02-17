-module(ty_function).

-ifndef(NODE).
-define(NODE, ty).
-endif.

-compile([export_all, nowarn_export_all]).

-export_type([type/0]).
-type type() :: {ty_function, [?NODE:type()], ?NODE:type()}.

-spec compare(type(), type()) -> lt | gt | eq.
compare({ty_function, Domains1, Codomain1}, {ty_function, Domains2, Codomain2}) ->
  true = length(Domains1) =:= length(Domains2),
  utils:compare(
    fun(Node1, Node2) -> 
      ?NODE:compare(Node1, Node2) end,
    Domains1 ++ [Codomain1], 
    Domains2 ++ [Codomain2]
  ).

-spec equal(type(), type()) -> boolean().
equal(T1, T2) -> compare(T1, T2) =:= 0.

function(Refs, Ref2) when is_list(Refs) ->
  {ty_function, Refs, Ref2}.

% ty_function -> ty_node
domain({ty_function, [SingleDomain], _}) -> SingleDomain;
domain({ty_function, _Domain, _}) ->
  error({need_tuples}).

codomain({ty_function, _, Codomain}) when not is_list(Codomain) -> Codomain.

% substitute({ty_function, Refs, B}, Map, Memo) ->
%     {ty_function,
%         lists:map(fun(C) -> ty_rec:substitute(C, Map, Memo) end, Refs),
%         ty_rec:substitute(B, Map, Memo)
%     }.
