%%
%% erlang out of memory
%%
%% eoom meta module
%%

-module(eoom).
-author({ "David J. Goehrig", "dave@dloh.org"}).
-copyright("© 2012 David J. Goehrig").
-export([ init/0 ]).

init() ->
	O = object:new(),
	O ! [ named, object ].		%% register base objec
