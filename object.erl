%%
%% erlang out of memory
%%
%% object 
%%

-module(object).
-author({ "David J. Goehrig", "dave@dloh.org"}).
-copyright("© 2012 David J. Goehrig").
-export([ new/0, new/1, init/1 ]).

%% Construct a new object
new() ->
	?MODULE:new(object).

new(Proto) ->
	spawn_link(?MODULE, init, [[ { prototype, Proto } ]]).

init(State) ->
	receive
		[ free ] -> 
			io:format("freeing ~p~n", [ self() ]);			%% we just dump our state and allow our process to end
		[ dump ] ->
			lists:map(fun({X,Y}) -> io:format("~p: ~p~n", [ X, Y]) end, State),
			init(State);
		%% define a member variable, usage:  Object ! [ Var, Delegate, Method ] 
		[ def, Var, Value ] when is_atom(Var) ->
			self() ! [ add, Var, fun(_Self,Pid,Method,Arg) -> Pid ! [ Method, Value, Arg ] end ],
			init(State);
		%% registers the object as a global process
		[ named, Name ] ->
			register(Name,self()),
			init(State);
		%% adds a method to the object, does not remove previous binding
		[ add, Method, Function ] when is_atom(Method), is_function(Function) ->
			State2 = [ { Method, Function } | State ],
			init(State2);
		%% removes a method from the object (does not affect prototypes, never resends!)
		[ remove, Method ] when is_atom(Method) ->
			State2 = proplists:delete(Method,State),
			init(State2);
		[ fetch, Method, Pid ] when is_atom(Method), is_pid(Pid) ->
			Pid ! [ fetched, proplists:get_value(Method,State) ],
			init(State);
		%% lookup does a parent method lookup.  It allows us to inspect
		[ lookup, Method, Pid | Args ] when is_atom(Method), is_pid(Pid) ->		%% Prototype method lookup
			Pid ! [ lookedup, proplists:get_value(Method,State) | Args ],		%% Return the module if we have it
			init(State);
		%% lookedup dispatches the prototypes method on ourself
		[ lookedup, Method | Args ] when is_function(Method) ->				%% Prototype looked up method
			apply(Method, [ self() | Args ]),					%% We call this as if it were ours
			init(State);
		%% Methods can't mutate object state! must use add/remove messages to mutate
		[ Method | Args ] when is_atom(Method) -> 	
			case proplists:is_defined(Method,State) of
				true -> apply(proplists:get_value(Method,State),[ self() | Args ]);		%% straight method call
				_ ->	Proto = proplists:get_value(prototype, State),
					Proto ! [ lookup, Method, self() | Args ]				%% beg our prototype for method
			end,
			init(State);
		_ ->				%% ignore things we do not know how to do
			init(State)
	end.
