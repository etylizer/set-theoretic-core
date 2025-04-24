-module(inductive).

-compile([export_all, nowarn_unused_type, nowarn_export_all]).

%  This module implements 7.1 of Frischs PhD thesis to study caching behavior of recursive systems.
%
%  p = 
%   t 
%   | 0
%   | 1
%   | p U p
%   | p & p
-type phi() :: 
  variable()
| 0
| 1
| {'or', phi(), phi()}
| {'and', phi(), phi()}.
-type system() :: #{variable() => phi()}.

-type variable() :: atom().
