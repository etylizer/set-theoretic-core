-module(musem).

%  This module implements a minimal but full implementation 
%  for the semantic subtyping framework for the limited type algebra:
%
%  t = 
%   flag
%   | {t, t} 
%   | !t 
%   | t U t 
%   | alpha 
%   | mu alpha . t
%
%  The recursion is explicitly defined via recursion variables and a recursion variable binder.
%  The resulting AST is finite and needs explicit unfolding.
%  Note that there is neither an explicit Any nor an explicit Empty type.
-type ast() :: 
  {flag} 
| {product, ast(), ast()} 
| {negation, ast()}
| {union, ast(), ast()}
| rec_var()
| {mu, rec_var(), ast()}.

%  Recursion variables are indentified by an integer or a name.
%  Usually, a named recursion binder is defined via a user-specified type,
%  and integers are reserved for recursive types generated by the algorithm.
-type var_name() :: atom() | integer().
-type rec_var() :: {recursion_variable, var_name()}.

%  We can now define some abbreviations.
%  The Any type is a recursive type that unions the flag and recursively the product of itself.
%  It has a fixed name and is denoted by 'any'.
-spec any() -> ast().
any() ->
  Any = {recursion_variable, any},
  {mu, Any, {union, {flag}, {product, Any, Any}}}.

%  The empty type is the negation of any.
-spec empty() -> ast().
empty() ->
  {negation, any()}.

%  Everytime we request the Any type, we get a syntactically equivalent Any type. 
%  But creating a semantically equivalent but syntactically different Any type is still possible.
%  There are many equivalence classes which one could consider for sharing types,
%  which are show-cased in this test. 
%  Some sharing is even required for the subtype algorithm to terminate, more on that later.
%    > rebar3 eunit --test musem:any_equivalence_classes_test
-spec any_equivalence_classes_test() -> ok.
any_equivalence_classes_test() ->
  Any = any(), 
  Any = any(), % same

  % equivalence modulo Boolean tautologies
  % e.g. A u A =:= A
  AnyTwice = {union, any(), any()},
  false = Any =:= AnyTwice,
  
  % equivalence modulo mu-renaming
  Var = {recursion_variable, any2},
  AnyMuEquivalent = {mu, Var, {union, {flag}, {product, Var, Var}}},
  false = Any =:= AnyMuEquivalent,

  % equivalence modulo unfolding
  AnyUnfoldedOnce = {union, {flag}, {product, any(), any()}},
  false = Any =:= AnyUnfoldedOnce,

  % ... other classes in between?
  
  % non-trivial semantic equivalence
  AnyEquiv = {union, {flag}, {union, any(), {product, {flag}, {flag}}}},
  false = Any =:= AnyEquiv,

  ok.

%  The subtype algorithm requires the expression to be in disjunctive normal form.
%  For that, we need to introduce the intersection operator.
%  We will not modify the ast. 
%  Instead, we define intersection as a macro expansion in terms of negation and union.
%  Intersections are syntactic sugar.
-define(INTERSECTION(A,B), {negation, {union, {negation, A}, {negation, B}}}).

%  Even more syntactic sugar to create more interesting types: Booleans.
-define(TRUE, {product, {flag}, any()}).
-define(FALSE, {product, any(), {flag}}).
-define(BOOL, {union, ?TRUE, ?FALSE}).

%  To make life a bit easier, 
%  we define a non-reversible pretty-printing function.
% 
%  Pretty-printing can involve custom type representations 
%  for a given syntactical structure of a type (e.g. the any type), 
%  or (further) semantically-equivalent simplification 
%  of the given type (see double negation elemination).
% 
%  This is the first function that works directly on a type.
%  TODO explain: why can this be implemented both recursively and, 
%                even if unnecessary, corecursively (with unfolding and memoization), 
%                whereas the subtype check *must* be implemented corecursively?
-spec print(ast()) -> term().
print(?INTERSECTION(A, B)) -> 
  % The messy 'raw' intersection with negations and unions 
  % is printed as a single intersection symbol.
  {print(A), '&', print(B)};
print(A) -> 
  % registry of type -> pretty printed type
  % This registry can be exposed globally 
  % and user-defined types can be added.
  PrettyRegistry = #{
    any() => any,
    {negation, any()} => empty,
    ?TRUE => 'true',
    ?FALSE => 'false',
    ?BOOL => 'bool'
  },
  case {PrettyRegistry, A} of
    {#{A := Pretty}, _} -> Pretty;
    % simplifications
    % Pretty printing can employ any desired amount of (potentially expensive) simplifications.
    % We eliminate double negation because it is easy to do.
    {_, {negation, {negation, S}}} -> print(S);
    % type constructors
    {_, {flag}} -> flag;
    {_, {product, S, T}} -> {print(S), print(T)};
    % type connectives
    {_, {negation, S}} -> {'!', print(S)};
    {_, {union, S, T}} -> {print(S), u, print(T)};
    % recursion equations
    {_, {recursion_variable, V}} -> V;
    {_, {mu, {recursion_variable, V}, T}} -> {V, '=', print(T)}
  end.

%    > rebar3 eunit --test musem:print_test
-spec print_test() -> ok.
print_test() ->
  any = (Any = print(any())), 
  io:format(user,"Any:~n~p~n", [Any]),
  
  Var = {recursion_variable, any2},
  AnyMuEquivalent = {mu, Var, {union, {flag}, {product, Var, Var}}},
  {V, '=', {flag, u, {V, V}}} = print(AnyMuEquivalent),
  io:format(user,"Pretty custom Any:~n~p~n", [print(AnyMuEquivalent)]),

  % custom type pretty printing
  True = ?TRUE,
  False = ?FALSE,
  Bool = ?BOOL,
  io:format(user,"True and False:~n~p and ~p~nTrue | False: ~p~n", [print(True), print(False), print(Bool)]),
  
  ok.

%  The subtype algorithm assumes a corecursive interpretation of the following grammar:
%  t = flag | {t, t} | !t | t U t 
%  This means neither recursive binders nor recursive variables
%  should ever be encountered during the algorithm.
%  Unfolding all top-level recursive binders once ensures this property.
%  For this, unfolding and mu-substitution is implemented,
%  transforming an ast() to ast_unfolded(), 
%  where mu binders and variables are always below type constructors.
%  Note that we take care that all transformations of ast() 
%  are still subtypes of ast() afterwards.
-type ast_unfolded() :: 
  {flag} 
| {product, ast(), ast()} 
| {negation, ast_unfolded()}
| {union, ast_unfolded(), ast_unfolded()}.

-spec unfold_toplevel_recursive_types(ast()) -> ast_unfolded().
%  We stop at the product type constructor and do not unfold the recursive type binders inside it
unfold_toplevel_recursive_types(P = {product, _, _}) -> P;
%  For a binder, we unfold once and continue recursing until a type constructor is reached
unfold_toplevel_recursive_types(Ty = {mu, Var, InnerTy}) -> 
  unfold_toplevel_recursive_types(substitute(InnerTy, {Var, Ty}));
%  For unions and negations, we continue recursing
unfold_toplevel_recursive_types({negation, T}) -> {negation, unfold_toplevel_recursive_types(T)};
unfold_toplevel_recursive_types({union, S, T}) -> {union, unfold_toplevel_recursive_types(S), unfold_toplevel_recursive_types(T)};
unfold_toplevel_recursive_types(T) -> 
  T.

-spec substitute(ast(), {rec_var(), ast()}) -> ast().
substitute(V, {Var, Replacement}) when V =:= Var -> Replacement;
substitute({flag}, _Map) -> {flag};
substitute({M, S}, Map) -> {M, substitute(S, Map)};
substitute(Ty = {mu, Var, _T}, {Var, _Replacement}) -> Ty; % stop
substitute({mu, V, T}, Map) -> {mu, V, substitute(T, Map)};
substitute({M, S, T}, Map) -> {M, substitute(S, Map), substitute(T, Map)};
substitute(T, _) -> T.

%    > rebar3 eunit --test musem:any_unfold_test
-spec any_unfold_test() -> ok.
any_unfold_test() ->
  Any = any(),
  any = print(Any),
  empty = print(empty()),
  {flag, u, {any, any}} = print(unfold_toplevel_recursive_types(Any)),
  % unfold non-existant variable
  {flag} = unfold_toplevel_recursive_types({mu, {recursion_variable, a}, {flag}}),
  
  % double unfold capture-avoiding
  AVar = {recursion_variable, avar},
  Ty = {mu, AVar, {mu, AVar, {product, AVar, {flag}}}},
  {product, {mu, AVar, {product, AVar, {flag}}}, {flag}} = unfold_toplevel_recursive_types(Ty),
  io:format(user,"Ty: ~p~n", [print(Ty)]),
  io:format(user,"Unfold: ~p~n", [print(unfold_toplevel_recursive_types(Ty))]),

  ok.

%  We now define our DNF data structure, which will be a subset of our 'ast()' type.
%  First, we first transform the structure to a negation normal form (NNF), 
%  where the negations are pushed to the atoms.
%  Then, we transform the NNF to the disjunctive normal form (DNF).
% 
%  It should be noted that negations that are part of the intersection 
%  are not considered negations in the NNF sense.
-type ast_nnf() :: 
  {flag} | {negation, {flag}}
| {product, ast(), ast()} | {negation, {product, ast(), ast()}} 
| ?INTERSECTION(ast_nnf(), ast_nnf())
| {union, ast_nnf(), ast_nnf()}.

%  As input we get an unfolded ast() type.
%  Negated intersection and negated unions are transformed with DeMorgan's Law
%  Recurse on intersection and unions
%  Double negation is eliminated
-spec to_nnf(ast_unfolded()) -> ast_nnf().
to_nnf({negation, ?INTERSECTION(A, B)}) -> {union, to_nnf({negation, A}), to_nnf({negation, B})};
to_nnf(?INTERSECTION(A, B)) -> ?INTERSECTION(to_nnf(A), to_nnf(B));
to_nnf({negation, {union, A, B}}) -> ?INTERSECTION(to_nnf({negation, A}), to_nnf({negation, B}));
to_nnf({union, A, B}) -> {union, to_nnf(A), to_nnf(B)};
to_nnf({negation, {negation, A}}) -> to_nnf(A);
to_nnf(A) -> A.


%  We restrict the tree of unions by forcing it to be a list-like type.
%  From now on, we will call these types lists (but not in the sense of Erlang lists).
-type ast_dnf() :: ast_clause() | {union, ast_clause(), ast_dnf()}.
%  Clauses are either a literal or a list of literals. 
-type ast_clause() :: ast_literal() | ?INTERSECTION(ast_literal(), ast_clause()).
%  Literals are either type constructors, or the negation thereof. 
-type ast_literal() :: 
  {flag} 
| {product, ast(), ast()} 
| {negation, {flag}} 
| {negation, {product, ast(), ast()}}.

%  The transformation applies the distributive rule recursively.
-spec to_dnf(ast_nnf()) -> ast_dnf().
to_dnf({union, A, B}) ->
  % For unions, union the two disjunctive normal forms.
  union_dnf(to_dnf(A), to_dnf(B));
to_dnf(?INTERSECTION(IA, IB)) ->
  % For intersections, a cartesian product is constructed.
  % transform to a list to be able to use Erlang list comprehensions
  UnionToList = fun U({union, A, B}) -> U(A) ++ U(B); U(A) -> [A] end,
  [X | Xs] = [ordered_intersection(Ai, Bi) || Ai <- UnionToList(to_dnf(IA)), Bi <- UnionToList(to_dnf(IB))],
  lists:foldl(fun(I1, I2) -> union_dnf(I1, I2) end, X, Xs);
to_dnf(A) ->
  A.


%  One important property of the DNF is that there are no duplicate literals in a clause.
%  This is needed for termination.
%  Strictly speaking, only intersecting with the 'any' type should not
%  lead to ever growing types.
%  We ensure this by implementing intersection with an order on the literals,
%  manually merging both lists according to the order.
-spec ordered_intersection(ast_clause(), ast_clause()) -> ast_clause().
ordered_intersection(?INTERSECTION(LiteralA, IntersectionsB), I = ?INTERSECTION(LiteralS, _)) when LiteralA < LiteralS ->
  ?INTERSECTION(LiteralA, ordered_intersection(IntersectionsB, I));
ordered_intersection(I = ?INTERSECTION(LiteralA, _), ?INTERSECTION(LiteralS, IntersectionsT)) when LiteralS < LiteralA ->
  ?INTERSECTION(LiteralS, ordered_intersection(IntersectionsT, I));
ordered_intersection(?INTERSECTION(Literal, IntersectionsB), ?INTERSECTION(Literal, IntersectionsT)) ->
  ?INTERSECTION(Literal, ordered_intersection(IntersectionsB, IntersectionsT));
ordered_intersection(Literal, I = ?INTERSECTION(LiteralS, _)) when Literal < LiteralS ->
  ?INTERSECTION(LiteralS, I);
ordered_intersection(Literal, ?INTERSECTION(LiteralS, IntersectionsT)) when LiteralS < Literal ->
  ?INTERSECTION(LiteralS, ordered_intersection(Literal, IntersectionsT));
ordered_intersection(Literal, I = ?INTERSECTION(Literal, _)) ->
  I;
ordered_intersection(I = ?INTERSECTION(_, _IntersectionsB), Intersections) ->
  ordered_intersection(Intersections, I);
ordered_intersection(LiteralA, LiteralS) when LiteralA < LiteralS ->
  ?INTERSECTION(LiteralA, LiteralS);
ordered_intersection(LiteralA, LiteralS) when LiteralA > LiteralS ->
  ?INTERSECTION(LiteralS, LiteralA);
ordered_intersection(Literal, Literal) ->
  Literal.

%  Union has no restriction on duplicates for termination.
-spec union_dnf(ast_dnf(), ast_dnf()) -> ast_dnf().
union_dnf({union, IntersectionsA, UnionsB}, U) ->
  {union, IntersectionsA, union_dnf(UnionsB, U)};
union_dnf(Intersections, U) ->
  {union, Intersections, U}.

%    > rebar3 eunit --test musem:to_dnf_test
-spec to_dnf_test() -> ok.
to_dnf_test() ->
  A = {product, {flag}, any()},
  B = {product, any(), {flag}},

  % order matters
  F = to_dnf(?INTERSECTION(A, B)),
  F = to_dnf(?INTERSECTION(B, A)),
  
  % !(A & B) == (!a u !b)
  (NNF = {union, {negation, A}, {negation, B}}) = to_nnf(Ty = {negation, ?INTERSECTION(A, B)}),
  io:format(user,"Ty: ~p~nNNF: ~p~n", [print(Ty), print(NNF)]),

  % !(((!P) -> Q) -> R) == ((!A & !B) & !R)
  R = {product, any(), any()},
  (NNF2 = ?INTERSECTION(?INTERSECTION({negation, A}, {negation, B}), {negation, R})) = to_nnf(Ty2 = {negation,{union, {union, {negation, {negation, A}}, B}, R}}),
  io:format(user,"Ty2: ~p~nNNF2: ~p~n", [print(Ty2), print(NNF2)]),

  % (A & (B U T)) == (A & B) U (A & T)
  T = {product, {flag}, {flag}},
  Ty3 = ?INTERSECTION(A, {union, B, T}),
  NNF3 = to_nnf(Ty3),
  io:format(user,"Ty3: ~p~nNNF3: ~p~n", [print(Ty3), print(NNF3)]),
  DNF3 = to_dnf(NNF3),
  io:format(user,"Dnf3: ~p~n", [print(DNF3)]),
  
  ok.

-type ast_split_dnf() :: ast_split_clause() | {union, ast_split_clause(), ast_split_dnf()}.
-type ast_split_clause() :: ast_split_clause({product, ast(), ast()}) | ast_split_clause({flag}).
-type ast_split_clause(Kind) :: ast_split_literal(Kind) | ?INTERSECTION(ast_split_literal(Kind), ast_split_clause(Kind)).
-type ast_split_literal(Kind) :: Kind | {negation, Kind}.

% TODO ugly, figure out how to avoid converting to lists
-spec intersection_to_list(ast_clause()) -> [ast_literal()].
intersection_to_list(?INTERSECTION(A, B)) ->
  intersection_to_list(A) ++ intersection_to_list(B);
intersection_to_list(A) -> [A].


-spec split_dnf(ast_dnf()) -> ast_split_dnf().
split_dnf({union, Clause, Unions}) ->
  D = split_dnf(Clause),
  union_dnf(D, split_dnf(Unions));
split_dnf(Clause) ->
  Cls = intersection_to_list(Clause),

  % positive and negative flags and products
  Pf = [X || X = {flag} <- Cls], 
  Nf = [X || X = {negation, {flag}} <- Cls],
  Pp = [X || X = {product, _A, _B} <- Cls], 
  Np = [X || X = {negation, {product, _A, _B}} <- Cls],

  Intersect = fun([X | Xs]) -> lists:foldl(fun ordered_intersection/2, X, Xs) end,

  case {Pf, Nf, Pp, Np} of
    % Two positives of a kind -> intersection empty -> create the simplest clause that is empty (we have no neutral element)
    {Pf, _, Pp, _} when length(Pf) > 0, length(Pp) > 0 -> 
      ?INTERSECTION({flag}, {negation, {flag}});
    % Only positive flags, ignore negative other kinds
    {Pf, Nf, [], _} when length(Pf) > 0 -> 
      Intersect(Pf ++ Nf);
    % Only positive products, ignore negative other kinds
    {[], _, Pp, Np} when length(Pp) > 0 -> 
      Intersect(Pp ++ Np);
    % Only negatives, create an ANY type and intersect the respective kinds with the negative components
    {[], Nf, [], Np} when (length(Nf) + length(Np)) > 0 -> 
      {union,
        Intersect([{flag}] ++ Nf),
        Intersect([{product, any(), any()}] ++ Np)
      }
  end.


-spec split_dnf_test() -> ok.
split_dnf_test() ->
  Split = split_dnf({negation, {flag}}),
  {{flag, '&', {'!', flag}},u,{any, any}} = print(Split),

  Split2 = split_dnf({product, any(), any()}),
  {any, any} = print(Split2),

  ok.

%  Now we can start implementing the subtype algorithm
%  1. Unfold all top-level recursive binders once, 
%     such that the mu binder is always only below a product type constructor
%  2. Convert the ast() term into an ast_dnf() term
%  3. Simplify each clause into other clauses according to the rules 
%     so that each clause has only one type of type constructor (here, either flags or products)
%  4. Solve until one disjunction is found to be non-empty
% 
%  It still needs to be decided where to meoize terms.
%  This is essentially an implementation detail.
%  We could memoize the whole ast() term right here
%  and check ast() term membership at the beginning of the is_empty call.
%  Alternatively, we could memoize deeper down for each type constructor separately.
%  Here, we choose to memoize at the time when products are decomposed.
%  The value mapping of memo() is the codefinition of is_empty.
-type memo() :: #{ast_split_clause({product, ast(), ast()}) => true}.
-spec is_empty(ast(), memo()) -> boolean().
is_empty(Ty, Memoized) ->
  Unfolded = unfold_toplevel_recursive_types(Ty),
  Dnf = to_dnf(to_nnf(Unfolded)),
  SplitDnf = split_dnf(Dnf),
  solve(SplitDnf, Memoized).

%  A generic clause traversal
-spec clause_fold(
  fun((ast_split_clause({flag})) -> Result), % flag processing
  fun((ast_split_clause({product, ast(), ast()})) -> Result), % product processing
  fun((Result, Result) -> Result), % join results of two processed clauses
  ast_split_dnf()) -> Result.
clause_fold(FlagFun, ProductFun, Join, {union, Clause, Unions}) ->
  Res = clause_fold(FlagFun, ProductFun, Join, Clause),
  Res2 = clause_fold(FlagFun, ProductFun, Join, Unions),
  Join(Res, Res2);
clause_fold(FlagFun, ProductFun, _Join, Clause) ->
  % first element in clause determines its category
  case Clause of
    % flags
    ?INTERSECTION(A, _B) when A == {flag} -> FlagFun(Clause);
    ?INTERSECTION(A, _B) when A == {negation, {flag}} -> FlagFun(Clause);
    A when A == {flag} -> FlagFun(Clause);
    A when A == {negation, {flag}} -> FlagFun(Clause);

    % products
    ?INTERSECTION({product, _, _}, _B)  -> ProductFun(Clause);
    ?INTERSECTION({negation, {product, _, _}}, _B)  -> ProductFun(Clause);
    {product, _, _}  -> ProductFun(Clause);
    {negation, {product, _, _}} -> ProductFun(Clause)
  end.


%  fold over all clause and solve each clause separately.
-spec solve(ast_split_dnf(), memo()) -> boolean().
solve(SplitDnf, Memoized) -> 
  clause_fold(
    % flags are not recursive and do not need memoization
    fun solve_flag/1,
    fun(ProductClause) -> 
      % products can be recursive and need memoization
      % memoization happens before dispatching to the product solve function
      case Memoized of
        #{ProductClause := true} -> true;
        _ -> 
          solve_product_clause(ProductClause, Memoized#{ProductClause => true}) 
      end
    end,
    % terminates even without shortcutting and semantics
    fun(R1, R2) -> R1 and R2 end,
    SplitDnf
  ).

%  A clause of flags is empty if it contains a negated flag.
-spec solve_flag(ast_split_clause({flag})) -> boolean().
solve_flag(FlagClause) ->
  Cls = intersection_to_list(FlagClause),
  lists:any(fun({negation, {flag}}) -> true; (_) -> false end, Cls).

%  A clause of products is empty if 
-spec solve_product_clause(ast_split_clause({product, ast(), ast()}), memo()) -> boolean().
solve_product_clause(ProductClause, Memoized) ->
  Cls = intersection_to_list(ProductClause),

  % simplification ensures there is always a positive product in a clause
  [FirstProduct | Ps] = [X || X = {product, _ ,_} <- Cls],
  Np = [X || X = {negation, {product, _, _}} <- Cls],

  {product, Ty1, Ty2} = lists:foldl(
    fun({product, P1, P2}, {product, T1, T2}) -> {product, ?INTERSECTION(P1, T1), ?INTERSECTION(P2, T2)} end, 
    FirstProduct, Ps),

  phi(Ty1, Ty2, Np, Memoized).

%  Φ(S1 , S2 , ∅ , T1 , T2) = (S1<:T1) or (S2<:T2)
%  Φ(S1 , S2 , N ∪ {T1, T2} , Left , Right) = Φ(S1 , S2 , N , Left | T1 , Right) and Φ(S1 , S2 , N , Left , Right | T2)
-spec phi({product, ast(), ast()}, {product, ast(), ast()}, [{negation, {product, ast(), ast()}}], memo()) -> boolean().
phi(S1, S2, N, Memo) -> 
  phi(S1, S2, N, empty(), empty(), Memo).

-spec phi({product, ast(), ast()}, {product, ast(), ast()}, [{negation, {product, ast(), ast()}}], ast(), ast(), memo()) -> boolean().
phi(S1, S2, [], T1, T2, Memo) ->
     is_empty(?INTERSECTION(S1, {negation, T1}), Memo) 
  or is_empty(?INTERSECTION(S2, {negation, T2}), Memo);
phi(S1, S2, [{negation, {product, T1, T2}} | N], Left, Right, Memo) ->
      phi(S1, S2, N, {union, Left, T1}, Right, Memo) 
  and phi(S1, S2, N , Left, {union, Right, T2}, Memo).


-spec is_empty_test() -> ok.
is_empty_test() ->
  % basic recursive types
  false = is_empty(any(), #{}),
  true = is_empty({negation, any()}, #{}),

  % split tests
  false = is_empty({flag}, #{}),
  false = is_empty({negation, {flag}}, #{}),
  false = is_empty({product, any(), any()}, #{}),
  false = is_empty({negation, {product, any(), any()}}, #{}),

  % empty products
  true = is_empty({product, empty(), any()}, #{}),
  true = is_empty({product, any(), empty()}, #{}),
  true = is_empty({product, empty(), empty()}, #{}),

  % anonymous recursive types
  % (X, (X, true)) where X = (X, true) | (true, true)
  XVar = {recursion_variable, x},
  X = {mu, XVar, {union, {product, XVar, {flag}}, {product, {flag}, {flag}}}},
  Ty0 = {product, X, {product, X, {flag}}},
  false = is_empty(Ty0, #{}),
  io:format(user,"~p~n", [print(Ty0)]),
  
  % mutually recursive types
  % (true, A) where 
  % A = (B, true) | (true, true)
  % B = (true, A)
  AVar = {recursion_variable, a},
  BVar = {recursion_variable, b},
  B = {mu, BVar, {product, {flag}, AVar}},
  A = {mu, AVar, {union, {product, B, {flag}}, {product, {flag}, {flag}}}},
  Ty = {product, {flag}, A},
  io:format(user,"~p~n", [print(Ty)]),
  false = is_empty(Ty, #{}),

  % X where 
  % X = (true, Y) | true
  % Y = X
  XVar2 = XVar,
  YVar2 = {recursion_variable, y},
  Y2 = {mu, YVar2, XVar2},
  X2 = {mu, XVar2, {union, {product, {flag}, Y2}, {flag}}},
  Ty2 = X2,
  false = is_empty(Ty2, #{}),
  io:format(user,"~p~n", [print(Ty2)]),

  ok.