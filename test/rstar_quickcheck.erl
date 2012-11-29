-module(rstar_quickcheck).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-include("../include/rstar.hrl").

-record(state, {
          tree = undefined,
          geos = []
         }).


main_test_() ->
    {timeout, 30, {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun search_2d/1
     ]}}.

setup() -> ok.
cleanup(_) -> ok.

search_2d(_) ->
    {timeout, 30, ?_test(
        begin
            ?assertEqual(true, proper:quickcheck(rstar_statem(),
                                                 [{numtests, 2000}, {to_file, user}]))
        end
    )}.

rstar_statem() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H,S,Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(
		   io:format("History: ~w\nState: ~w\nRes: ~w\n",
			     [H, S, Res]),
		   aggregate(command_names(Cmds), Res =:= ok))
	    end).


% Returns a random geo point
random_geo() ->
    rstar_util:random_geo(2, 10000).

% Returns a random distance
random_distance() ->
    range(1,1000).


% Initial state creates a tree with no geometries
initial_state() ->
    % Reduce the fan-out so we get taller trees. This
    % is so that we can build more complex trees faster.
    Params = #rt_params{max=8, min=2, reinsert=3},
    #state{tree=rstar:new(2, Params)}.


% Picks to either do an insert, delete or search near
command(#state{tree=Tree, geos=Geos}) ->
    weighted_union([
        {6, {call, rstar, insert, [Tree, random_geo()]}},
        {min(1, length(Geos)), {call, rstar, delete, [Tree, oneof(Geos)]}},
        {1, {call, rstar, delete, [Tree, random_geo()]}},
        {2, {call, rstar, search_around, [Tree, random_geo(), random_distance()]}}
    ]).


% A precondition of delete is that the geo exists
precondition(#state{geos=Geos}, {call, rstar, delete, [_, G]}) ->
    lists:member(G, Geos);

% No preconditions for insert or search
precondition(_State, _Call) ->
    true.


% A post conidtion of delete is that the geo must not be added if we get not_found
postcondition(#state{geos=Geos}, {call, rstar, delete, [_, G]}, not_found) ->
    not lists:member(G, Geos);

% A post conidtion of search_nearest is that it must match the list of geometries that
% are within that distance
postcondition(#state{geos=Geos}, {call, rstar, search_nearest, [_, Search, Dist]}, Results) ->
    % Sort the results
    SortedRes = lists:sort(Results),

    % Filter on distance
    Expected = lists:sort(lists:filter(fun(G) ->
        rstar_geometry:distance(Search, G) =< Dist
    end, Geos)),

    % Expect the two to be equal
    SortedRes == Expected;

% No post condition on insert
postcondition(_S, _C, _R) ->
    true.

% Insert should add a geo
next_state(State=#state{geos=Geos}, Tree, {call, rstar, insert, [_, G]}) ->
    NewGeos = [G | Geos],
    State#state{tree=Tree, geos=NewGeos};

% Delete may result in nothing
next_state(State, not_found, {call, rstar, delete, _}) ->
    State;

% Delete should remove a geo
next_state(State=#state{geos=Geos}, Tree, {call, rstar, delete, [_, G]}) ->
    NewGeos = Geos -- [G],
    State#state{tree=Tree, geos=NewGeos};

% Doing a search causes no state change
next_state(State, _V, _C) ->
    State.


