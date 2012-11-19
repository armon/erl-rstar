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
      fun bad_axis/1,
      fun correct_input/1,
      fun origin_no_dimensions/1,
      fun origin_2d/1,
      fun point2d/1,
      fun point3d/1,
      fun bounding_2d/1,
      fun bounding_3d/1,
      fun area/1
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

bad_axis(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg},
                         rstar_geometry:new(2, [{1, 2}, {3, 2}], test))
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

origin_no_dimensions(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg}, rstar_geometry:origin(0))
        end
    ).

origin_2d(_) ->
    ?_test(
        begin
            Expect = #geometry{dimensions=2, mbr=[{0,0}, {0,0}]},
            ?assertEqual(Expect, rstar_geometry:origin(2))
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

bounding_2d(_) ->
    ?_test(
        begin
            BBExpect = #geometry{dimensions=2, mbr=[{0,2}, {0,2}],
                               value=undefined},
            Geo1 = rstar_geometry:point2d(0, 0, test),
            Geo2 = rstar_geometry:point2d(2, 2, foobar),
            ?assertEqual(BBExpect,
                         rstar_geometry:bounding_box([Geo1, Geo2]))
        end
    ).

bounding_3d(_) ->
    ?_test(
        begin
            BBExpect = #geometry{dimensions=3, mbr=[{0,3}, {0,3}, {0,3}],
                               value=undefined},
            Geo1 = rstar_geometry:point3d(0, 0, 0, test),
            Geo2 = rstar_geometry:point3d(1, 1, 1, foobar),
            Geo3 = rstar_geometry:point3d(3, 3, 3, bar),
            ?assertEqual(BBExpect,
                         rstar_geometry:bounding_box([Geo1, Geo2, Geo3]))
        end
    ).

area(_) ->
    ?_test(
        begin
            Geo2d = #geometry{dimensions=2, mbr=[{0,2}, {0,2}],
                               value=undefined},
            ?assertEqual(4, rstar_geometry:area(Geo2d)),

            Geo3d = #geometry{dimensions=2, mbr=[{0,2}, {0,2}, {0,8}],
                               value=undefined},
            ?assertEqual(32, rstar_geometry:area(Geo3d))

        end
    ).

