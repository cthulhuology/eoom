%%
%% erlang out of memory
%%
%% object 
%%

-module(object).
-author({ "David J. Goehrig", "dave@dloh.org"}).
-copyright("© 2012 David J. Goehrig").
-export([ new/0, new/1, init/1 ]).

new() ->
	?MODULE:new(object).

new(Proto) ->
	spawn_link(?MODULE, init, [ { prototype, Proto } ]).

init(State) ->
	receive
		{ addMethod, Method, Function } ->
			State2 = [ { Method, Function } | State ],
			init(State2);
		{ removeMethod, Method } ->
			State2 = proplists:delete(Method,State),
			init(State2);
		[ Method | Args ] -> 
			State2 = case proplists:is_defined(Method,State) of
				true -> erlang:apply(?MODULE,Method,Args);
				_ -> erlang:apply(proplists:get_value(prototype, State),Method,Args)
			end,
			init(State2)
	end.
