% A generic BDD parameterized over both the 'nodes and 'leafs
% 
% hide built-in Erlang node function
-compile([export_all, nowarn_export_all]).
-compile({no_auto_import, [node/1]}).

-ifndef(ATOM).
-define(ATOM, ty_bool).
-endif.
-ifndef(LEAF).
-define(LEAF, ty_bool).
-endif.

-type dnf() :: term(). % TODO
-type bdd() ::
  {leaf, ?LEAF:type()}
  | {node, _Atom :: ?ATOM:type(), _PositiveEdge :: bdd(), _NegativeEdge :: bdd()}.

-spec any() -> bdd().
any() -> {leaf, ?LEAF:any()}.

-spec empty() -> bdd().
empty() -> {leaf, ?LEAF:empty()}.

-spec singleton(?ATOM:type()) -> bdd().
singleton(Atom) -> {node, Atom, any(), empty()}.

-spec negated_singleton(?ATOM:type()) -> bdd().
negated_singleton(Atom) -> {node, Atom, empty(), any()}.

-spec leaf(?LEAF:type()) -> bdd().
leaf(Leaf) -> {leaf, Leaf}.

-spec equal(bdd(), bdd()) -> bdd().
equal({node, A1, P1, N1}, {node, A2, P2, N2}) ->
  ?ATOM:equal(A1, A2) andalso equal(P1, P2) andalso equal(N1, N2);
equal({leaf, T1}, {leaf, T2}) ->
  ?LEAF:equal(T1, T2);
equal(_, _) ->
  false.

-spec compare(bdd(), bdd()) -> lt | gt | eq.
compare({leaf, T1}, {leaf, T2}) -> ?LEAF:compare(T1, T2);
compare({leaf, _}, {node, _, _, _}) -> lt;
compare({node, _, _, _}, {leaf, _}) -> gt;
compare({node, A1, P1, N1}, {node, A2, P2, N2}) ->
  case ?ATOM:compare(A1, A2) of
    eq ->
      case compare(P1, P2) of
        eq -> compare(N1, N2);
        Res -> Res
      end;
    Res -> Res
  end.

-spec negate(bdd()) -> bdd().
negate({leaf, A}) ->
  {leaf, ?LEAF:negate(A)};
negate({node, Atom, Pos, Neg}) -> 
  {node, Atom, negate(Pos), negate(Neg)}.


% simplification for BDDs
% implements a simple form of structural subsumption
% which is not apparant at first glance
% TODO example here
-spec normalize(bdd()) -> bdd().
normalize(Bdd = {node, _Atom, Pos, Neg}) -> 
  case equal(Pos, Neg) of
    true -> Pos;
    false -> Bdd
  end;
normalize(X) -> X.

-spec op(fun((?LEAF:type(), ?LEAF:type()) -> ?LEAF:type()), bdd(), bdd()) -> bdd().
op(LeafOperation, Bdd1, Bdd2) ->
  Op = fun ROp(T1, T2) ->
    Res = 
    case {T1, T2} of
      {{leaf, L1}, {leaf, L2}} -> {leaf, LeafOperation(L1, L2)};
      {{leaf, L}, {node, A, P, N}} -> 
        {node, A, ROp({leaf, L}, P), ROp({leaf, L}, N)};
      {{node, A, P, N}, {leaf, L}} -> 
        {node, A, ROp(P, {leaf, L}), ROp(N, {leaf, L})};
      {{node, A1, P1, N1}, {node, A2, P2, N2}} ->
        case ?ATOM:compare(A1, A2) of
          lt ->
            {node, A1, ROp(P1, T2), ROp(N1, T2)};
          gt ->
            {node, A2, ROp(T1, P2), ROp(T1, N2)};
          eq ->
            {node, A1, ROp(P1, P2), ROp(N1, N2)}
        end
    end,
    normalize(Res)
  end,
  Op(Bdd1, Bdd2).

-spec union(bdd(), bdd()) -> bdd().
union(T1, T2) -> 
  op(fun ?LEAF:union/2, T1, T2).

-spec intersect(bdd(), bdd()) -> bdd().
intersect(T1, T2) -> op(fun ?LEAF:intersect/2, T1, T2).

-spec difference(bdd(), bdd()) -> bdd().
difference(T1, T2) -> op(fun ?LEAF:difference/2, T1, T2).

-spec dnf(bdd()) -> dnf().
dnf(T) ->
  dnf_acc([], [], [], T).

-spec dnf_acc(dnf(), [?ATOM:type()], [?ATOM:type()], bdd()) -> dnf().
dnf_acc(Acc, Ps, Ns, {leaf, T}) ->
  [{Ps, Ns, T} | Acc];
dnf_acc(Acc, Ps, Ns, {node, A, P, N}) ->
  % TODO small heuristic add
  Acc0 = dnf_acc(Acc, [A | Ps], Ns, P),
  dnf_acc(Acc0, Ps, [A | Ns], N).


% is_empty_union(F1, F2) ->
%   F1() andalso F2().

% get_dnf(Bdd) ->
%   lists:filter(
%     fun({_,_,[]}) -> false; ({_, _, T}) ->
%       case ?TERMINAL:empty() of
%         T -> false;
%         _ ->  true
%       end
%     end,
%     dnf(Bdd, {fun(P, N, T) -> [{P, N, T}] end, fun(C1, C2) -> C1() ++ C2() end})
%   ).

% dnf(Bdd, {ProcessCoclause, CombineResults}) ->
%   do_dnf(Bdd, {ProcessCoclause, CombineResults}, _Pos = [], _Neg = []).

% do_dnf({node, Element, Left, Right}, F = {_Process, Combine}, Pos, Neg) ->
%   % heuristic: if Left is positive & 1, skip adding the negated Element to the right path
%   % TODO can use the see simplifications done in ty_rec:transform to simplify DNF before processing?
%   case {terminal, ?TERMINAL:any()} of
%     Left ->
%       F1 = fun() -> do_dnf(Left, F, [Element | Pos], Neg) end,
%       F2 = fun() -> do_dnf(Right, F, Pos, Neg) end,
%       Combine(F1, F2);
%     _ ->
%       F1 = fun() -> do_dnf(Left, F, [Element | Pos], Neg) end,
%       F2 = fun() -> do_dnf(Right, F, Pos, [Element | Neg]) end,
%       Combine(F1, F2)
%   end;
% do_dnf({terminal, Terminal}, {Proc, _Comb}, Pos, Neg) ->
%   Proc(Pos, Neg, Terminal).

