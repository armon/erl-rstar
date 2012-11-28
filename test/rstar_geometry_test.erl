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
      fun area/1,
      fun no_intersect/1,
      fun intersect_2d/1,
      fun intersect_3d/1,
      fun num_edges_test/1,
      fun margin_test/1,
      fun center_2d_test/1,
      fun center_3d_test/1,
      fun distance_2d_test/1,
      fun distance_3d_test/1,
      fun min_distance_2d_test/1,
      fun min_distance_3d_test/1
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
            ?assertEqual(32, rstar_geometry:area(Geo3d)),

            GeoPoint = #geometry{dimensions=2, mbr=[{3,3}, {3,3}],
                               value=undefined},
            ?assertEqual(0, rstar_geometry:area(GeoPoint))
        end
    ).

no_intersect(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}],
                               value=undefined},

            Geo2 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}],
                               value=undefined},

            ?assertEqual(undefined, rstar_geometry:intersect(Geo1, Geo2))
        end
    ).

intersect_2d(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}],
                               value=tubez},

            Geo2 = #geometry{dimensions=2, mbr=[{1,3}, {0,2}],
                               value=bar},

            Expect = #geometry{dimensions=2, mbr=[{1,2}, {0,2}],
                               value=undefined},

            ?assertEqual(Expect, rstar_geometry:intersect(Geo1, Geo2))
        end
    ).

intersect_3d(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=3, mbr=[{0,3}, {0,3}, {0,3}],
                               value=tubez},

            Geo2 = #geometry{dimensions=3, mbr=[{2,5}, {2,5}, {2, 5}],
                               value=bar},

            Expect = #geometry{dimensions=3, mbr=[{2,3}, {2,3}, {2, 3}],
                               value=undefined},

            ?assertEqual(Expect, rstar_geometry:intersect(Geo1, Geo2))
        end
    ).

num_edges_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0,3}, {0,3}]},
            Geo2 = #geometry{dimensions=3, mbr=[{2,5}, {2,5}, {2, 5}]},
            Geo3 = #geometry{dimensions=4, mbr=[{2,5}, {2,5}, {2, 5}, {0, 10}]},

            ?assertEqual(4, rstar_geometry:num_edges(Geo1)),
            ?assertEqual(12, rstar_geometry:num_edges(Geo2)),
            ?assertEqual(32, rstar_geometry:num_edges(Geo3))
        end
    ).

margin_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0,3}, {0,3}]},
            Geo2 = #geometry{dimensions=3, mbr=[{2,5}, {2,5}, {2, 5}]},
            Geo3 = #geometry{dimensions=4, mbr=[{2,5}, {2,5}, {2, 5}, {0, 10}]},

            ?assertEqual(12.0, rstar_geometry:margin(Geo1)),
            ?assertEqual(36.0, rstar_geometry:margin(Geo2)),
            ?assertEqual(152.0, rstar_geometry:margin(Geo3))
        end
    ).

center_2d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0,3}, {0,3}], value=foo},
            Expect = #geometry{dimensions=2, mbr=[{1.5, 1.5}, {1.5, 1.5}], value=undefined},
            ?assertEqual(Expect, rstar_geometry:center(Geo1))
        end
    ).

center_3d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=3, mbr=[{2,4}, {2,4}, {0, 10}], value=foo},
            Expect = #geometry{dimensions=3, mbr=[{3.0, 3.0}, {3.0, 3.0}, {5.0, 5.0}], value=undefined},
            ?assertEqual(Expect, rstar_geometry:center(Geo1))
        end
    ).

distance_2d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0, 0}, {0, 0}]},
            Geo2 = #geometry{dimensions=2, mbr=[{4, 4}, {4, 4}]},
            ?assertEqual(0.0, rstar_geometry:distance(Geo1, Geo1)),
            ?assertEqual(5.656854249492381, rstar_geometry:distance(Geo1, Geo2))
        end
    ).

distance_3d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=3, mbr=[{0, 0}, {0, 0}, {0, 0}]},
            Geo2 = #geometry{dimensions=3, mbr=[{4, 4}, {4, 4}, {4, 4}]},
            ?assertEqual(0.0, rstar_geometry:distance(Geo1, Geo1)),
            ?assertEqual(6.928203230275509, rstar_geometry:distance(Geo1, Geo2))
        end
    ).

min_distance_2d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=2, mbr=[{0, 0}, {0, 0}]},
            Geo2 = #geometry{dimensions=2, mbr=[{2, 4}, {2, 4}]},
            Geo3 = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}]},
            ?assertEqual(0.0, rstar_geometry:min_dist(Geo2, Geo2)),
            ?assertEqual(8.0, rstar_geometry:min_dist(Geo1, Geo2)),
            ?assertEqual(0.0, rstar_geometry:min_dist(Geo3, Geo2))
        end
    ).

min_distance_3d_test(_) ->
    ?_test(
        begin
            Geo1 = #geometry{dimensions=3, mbr=[{0, 0}, {0, 0}, {0, 0}]},
            Geo2 = #geometry{dimensions=3, mbr=[{2, 4}, {2, 4}, {2, 4}]},
            Geo3 = #geometry{dimensions=3, mbr=[{3, 3}, {3, 3}, {3, 3}]},
            ?assertEqual(0.0, rstar_geometry:min_dist(Geo2, Geo2)),
            ?assertEqual(12.0, rstar_geometry:min_dist(Geo1, Geo2)),
            ?assertEqual(0.0, rstar_geometry:min_dist(Geo3, Geo2))
        end
    ).

