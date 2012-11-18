-module(rstar_geometry_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun new_zero_dimension/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

new_zero_dimension(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg},
                         rstart_geometry:new(0, [], test))
        end
    ).


