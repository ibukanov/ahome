-module(test).
-export([for/3]).

for(From, To, F) -> for(From, To, F, []).

for(From, To, _, List) when From > To -> List ;
for(From, To, F, List) -> for(From, To - 1, F, [ F(To) | List ]).
