-module(rstar_search_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
    fun setup/0,
    fun cleanup/1,
    [
    fun search_within_leaf_test/1,
    fun search_within_node_test/1,
    fun search_near_leaf_test/1,
    fun search_near_node_test/1
    ]
    }.

setup() -> ok.
cleanup(_) -> ok.

search_within_leaf_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{3,3}, {3,3}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{7,7}, {7,7}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{10,10}, {10,10}], value=#leaf{}},

            N1Geo = rstar_geometry:bounding_box([L1, L2, L3, L4]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L1, L2, L3, L4]}},

            Search = #geometry{dimensions=2, mbr=[{2,8}, {2,8}]},

            ?assertEqual([L3, L2],
                         rstar_search:search_within(N1, Search))
        end
    ).

search_within_node_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}]},
            L2 = #geometry{dimensions=2, mbr=[{3,3}, {3,3}]},
            L3 = #geometry{dimensions=2, mbr=[{7,7}, {7,7}]},
            L4 = #geometry{dimensions=2, mbr=[{10,10}, {10,10}]},

            N1Geo = rstar_geometry:bounding_box([L1, L2]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L1, L2]}},

            N2Geo = rstar_geometry:bounding_box([L3, L4]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L3, L4]}},

            R1Geo = rstar_geometry:bounding_box([N1, N2]),
            R1 = R1Geo#geometry{value=#node{children=[N1, N2]}},

            Search = #geometry{dimensions=2, mbr=[{2,8}, {2,8}]},

            ?assertEqual([L3, L2],
                         rstar_search:search_within(R1, Search))
        end
    ).

search_near_leaf_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{3,3}, {3,3}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{7,7}, {7,7}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{10,10}, {10,10}], value=#leaf{}},

            N1Geo = rstar_geometry:bounding_box([L1, L2, L3, L4]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L1, L2, L3, L4]}},

            Search = #geometry{dimensions=2, mbr=[{2,2}, {2,2}]},

            ?assertEqual([L2, L1, L3, L4],
                         rstar_search:search_near(N1, Search, 4)),

            ?assertEqual([L2, L1, L3],
                         rstar_search:search_near(N1, Search, 3)),

            ?assertEqual([L2, L1],
                         rstar_search:search_near(N1, Search, 2)),

            ?assertEqual([L2],
                         rstar_search:search_near(N1, Search, 1))
        end
    ).

search_near_node_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}]},
            L2 = #geometry{dimensions=2, mbr=[{3,3}, {3,3}]},
            L3 = #geometry{dimensions=2, mbr=[{7,7}, {7,7}]},
            L4 = #geometry{dimensions=2, mbr=[{10,10}, {10,10}]},

            N1Geo = rstar_geometry:bounding_box([L1, L2]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L1, L2]}},

            N2Geo = rstar_geometry:bounding_box([L3, L4]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L3, L4]}},

            R1Geo = rstar_geometry:bounding_box([N1, N2]),
            R1 = R1Geo#geometry{value=#node{children=[N1, N2]}},

            Search = #geometry{dimensions=2, mbr=[{2,8}, {2,8}]},

            ?assertEqual([L2, L1, L3, L4],
                         rstar_search:search_near(R1, Search, 4)),

            ?assertEqual([L2, L1, L3],
                         rstar_search:search_near(R1, Search, 3)),

            ?assertEqual([L2, L1],
                         rstar_search:search_near(R1, Search, 2)),

            ?assertEqual([L2],
                         rstar_search:search_near(R1, Search, 1))
        end
    ).

