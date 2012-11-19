-module(rstar_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun new_bad_dimension/1,
      fun new_valid/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

new_bad_dimension(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg}, rstar:new(0)),
            ?assertEqual({error, badarg}, rstar:new(1))
        end
    ).

new_valid(_) ->
    ?_test(
        begin
            Root1 = rstar_geometry:origin(2),
            DefParams = #rt_params{},
            Root2 = Root1#geometry{value=#leaf{}},
            Tree = #rtree{dimensions=2, params=DefParams,root=Root2},
            ?assertEqual(Tree, rstar:new(2))
        end
    ).

