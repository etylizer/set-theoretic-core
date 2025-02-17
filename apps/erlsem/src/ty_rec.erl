-module(ty_rec).

-compile([export_all, nowarn_export_all]).


% TODO atoms
-record(ty, 
  {
    % atom, 
    ty_functions
  }).

-type type() :: #ty{}.

-define(RECORD, ty).
-include("utils/record_utils.hrl").


compare(Ty1, Ty2) ->
  binary_fold(fun
        (Module, V1, V2, eq) -> Module:compare(V1, V2);
        (_, _, _, R) -> R
      end, 
      eq,
      Ty1, Ty2).

-spec any() -> type().
any() ->
  map(fun(Field, _Value) -> Field:any() end, #ty{}).

-spec empty() -> type().
empty() ->
  map(fun(Field, _Value) -> Field:empty() end, #ty{}).

-spec is_empty(type()) -> boolean().
is_empty(Ty) ->
  fold(fun
        (_, _, true) -> true;
        (Module, Value, false) -> Module:is_empty(Value)      
      end, 
      false,
      Ty).

-spec negate(type()) -> type().
negate(T1) ->
  map(fun(Module, Value) -> Module:negate(Value) end, T1).

-spec union(type(), type()) -> type().
union(T1, T2) ->
  binary_map(fun(Module, Left, Right) -> Module:union(Left, Right) end, T1, T2).

-spec intersect(type(), type()) -> type().
intersect(T1, T2) ->
  binary_map(fun(Module, Left, Right) -> Module:intersect(Left, Right) end, T1, T2).

difference(T1, T2) ->
  binary_map(fun(Module, Left, Right) -> Module:difference(Left, Right) end, T1, T2).

functions(Fs) ->
  (empty())#ty{ty_functions = Fs}.
