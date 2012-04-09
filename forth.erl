%%
%% erlang out of memory
%%
%% a Forth object in eoom
%%

-module(forth).
-author({ "David J. Goehrig", "dave@dloh.org"}).
-copyright("© 2012 David J. Goehrig").
-export([ new/0 ]).

new() ->
	Ok = object:new(),
	Ok ! [ def, stack, [] ],
	Ok ! [ add, lit, fun(Self,X) -> Self ! [ stack, Self, push, X ] end],
	Ok ! [ add, push, fun(Self,Stack,Value) -> Self ! [ def, stack, [ Value | Stack ]] end],
	Ok ! [ add, pop, fun(Self,[ _Top | Stack ]) -> Self ! [ def, stack, Stack ] end],
	Ok ! [ add, dump, fun(_Self,Stack,_Arg) -> io:format("Stack: ~p~n", [ Stack ]) end],
	Ok ! [ add, '+', fun(Self) -> Self ! [ stack, Self, plus, 0 ] end ],
	Ok ! [ add, plus, fun(Self, [ Top, Next | Stack], _Value) -> Self ! [ def, stack, [ Top + Next | Stack ]] end ],
	Ok ! [ add, '*', fun(Self) -> Self ! [ stack, Self, times, 0 ] end ],
	Ok ! [ add, times, fun(Self, [ Top, Next | Stack], _Value) -> Self ! [ def, stack, [ Top * Next | Stack ]] end ],
	Ok ! [ add, '-', fun(Self) -> Self ! [ stack, Self, minus, 0 ] end ],
	Ok ! [ add, minus, fun(Self, [ Top, Next | Stack], _Value) -> Self ! [ def, stack, [ Next - Top | Stack ]] end ],
	Ok ! [ add, '/', fun(Self) -> Self ! [ stack, Self, divide, 0 ] end ],
	Ok ! [ add, divide, fun(Self, [ Top, Next | Stack], _Value) -> Self ! [ def, stack, [ Next div Top | Stack ]] end ],
	Ok ! [ add, '%', fun(Self) -> Self ! [ stack, Self, mod, 0 ] end ],
	Ok ! [ add, mod, fun(Self, [ Top, Next | Stack], _Value) -> Self ! [ def, stack, [ Next rem Top | Stack ]] end ],
	Ok ! [ add, dup, fun(Self) -> Self ! [ stack, Self, duplicate, 0 ] end ],
	Ok ! [ add, duplicate, fun(Self, [ Top | Stack ], _Value) -> Self ! [ def, stack, [ Top, Top | Stack ]] end ],
	Ok ! [ add, swap, fun(Self) -> Self ! [ stack, Self, swaptwo, 0 ] end ],
	Ok ! [ add, swaptwo, fun(Self, [ Top, Next | Stack ], _Value) -> Self ! [ def, stack, [ Next, Top | Stack ]] end ],
	Ok.
