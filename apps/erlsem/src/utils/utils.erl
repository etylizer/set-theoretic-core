-module(utils).

-compile([export_all, nowarn_export_all]).

-spec compare (fun((T, T) -> lt | gt | eq), [T], [T]) -> lt | gt | eq.
compare(_Cmp, [], []) -> eq;
compare(Cmp, [T1 | Ts1], [T2 | Ts2]) ->
  case Cmp(T1, T2) of
    eq -> compare(Cmp, Ts1, Ts2);
    R -> R
  end.

% -spec equal (fun((T, T) -> boolean()), [T], [T]) -> boolean().
% equal(_Eq, [], []) -> eg;
% equal(Eq, [T1 | Ts1], [T2 | Ts2]) ->
%   case Eq(T1, T2) of
%     true -> equal(Eq, Ts1, Ts2);
%     false -> false
%   end.

% -spec everywhere(fun((term()) -> t:opt(term())), T) -> T.
everywhere(F, T) ->
    TransList = fun(L) -> lists:map(fun(X) -> everywhere(F, X) end, L) end,
    case F(T) of
        error ->
            case T of
                X when is_list(X) -> TransList(X);
                X when is_tuple(X) -> list_to_tuple(TransList(tuple_to_list(X)));
                X when is_map(X) -> maps:from_list(TransList(maps:to_list(X)));
                X -> X
            end;
        {ok, X} -> X;
        {rec, X} -> everywhere(F, X)
    end.