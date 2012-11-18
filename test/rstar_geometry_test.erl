-module(rstar_geometry_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun new_zero_dimension/1,
      fun missing_axis/1,
      fun correct_input/1,
      fun point2d/1,
      fun point3d/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

new_zero_dimension(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg},
                         rstar_geometry:new(0, [], test))
        end
    ).

missing_axis(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg},
                         rstar_geometry:new(2, [{1, 2}], test))
        end
    ).

correct_input(_) ->
    ?_test(
        begin
            Expect = #geometry{dimensions=2, mbr=[{1,2}, {3,4}], value=foo},
            ?assertEqual(Expect,
                         rstar_geometry:new(2, [{1, 2}, {3, 4}], foo))
        end
    ).

point2d(_) ->
    ?_test(
        begin
            Expect = #geometry{dimensions=2, mbr=[{1,1}, {2,2}], value=foo},
            ?assertEqual(Expect,
                         rstar_geometry:point2d(1, 2, foo))
        end
    ).

point3d(_) ->
    ?_test(
        begin
            Expect = #geometry{dimensions=3, mbr=[{1,1}, {2,2}, {3, 3}],
                               value=foo},
            ?assertEqual(Expect,
                         rstar_geometry:point3d(1, 2, 3, foo))
        end
    ).

