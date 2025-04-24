-module(stack).

-compile([export_all, nowarn_export_all]).
-export_type([stack/1]).

-opaque stack(T) :: [T].

-spec new() -> stack(_).
new() -> [].

-spec push(T, stack(T)) -> stack(T).
push(Value, OldStack) -> [Value|OldStack].

-spec pop(stack(T)) -> {T, stack(T)} | empty.
pop([]) -> empty;
pop([Value|RestStack]) -> {Value, RestStack}.
