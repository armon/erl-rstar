-module(rstar_insert_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun zero_overlap_test/1,
      fun some_overlap_test/1,
      fun some_overlap_3d_test/1,
      fun minimal_overlap_delta_test/1,
      fun minimal_overlap_delta_tie_test/1,
      fun minimal_area_delta_test/1,
      fun minimal_area_delta_tie_test/1,
      fun minimal_area_test/1,
      fun minimal_area_tie_test/1,
      fun strip_delta_test/1,
      fun choose_subtree_leaf_test/1,
      fun choose_subtree_one_level_test/1,
      fun choose_subtree_two_level_test/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

zero_overlap_test(_) ->
    ?_test(
        begin
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}]},
            ?assertEqual(0,
                         rstar_insert:overlap(G1, [G1, G2]))
        end
    ).

some_overlap_test(_) ->
    ?_test(
        begin
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}]},
            G2 = #geometry{dimensions=2, mbr=[{1,3}, {0,2}]},
            ?assertEqual(2,
                         rstar_insert:overlap(G1, [G1, G2]))
        end
    ).

some_overlap_3d_test(_) ->
    ?_test(
        begin
            G1 = #geometry{dimensions=3, mbr=[{0,2}, {0,2}, {0,2}]},
            G2 = #geometry{dimensions=3, mbr=[{1,3}, {0,2}, {0,2}]},
            ?assertEqual(4,
                         rstar_insert:overlap(G1, [G1, G2]))
        end
    ).

minimal_overlap_delta_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,2}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}]},
            ?assertEqual([{0, G1}],
                         rstar_insert:minimal_overlap_delta_helper(G0, [G1, G2]))
        end
    ).

minimal_overlap_delta_tie_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,2}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,2}]},
            G2 = #geometry{dimensions=2, mbr=[{2,4}, {0,2}]},
            G3 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}]},

            ?assertEqual([{2, G1}, {2, G2}],
                         rstar_insert:minimal_overlap_delta_helper(G0, [G1, G2, G3]))
        end
    ).

minimal_area_delta_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,4}]},
            ?assertEqual([{4, G1}],
                         rstar_insert:minimal_area_delta_helper(G0, [G1, G2]))
        end
    ).

minimal_area_delta_tie_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{2,4}, {0,4}]},
            G3 = #geometry{dimensions=2, mbr=[{4,6}, {0,4}]},

            ?assertEqual([{4, G1}, {4, G2}],
                         rstar_insert:minimal_area_delta_helper(G0, [G1, G2, G3]))
        end
    ).


minimal_area_test(_) ->
    ?_test(
        begin
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}]},
            ?assertEqual([{4, G2}],
                         rstar_insert:minimal_area_helper([G1, G2]))
        end
    ).

minimal_area_tie_test(_) ->
    ?_test(
        begin
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{2,6}, {0,2}]},

            ?assertEqual([{8, G1}, {8, G2}],
                         rstar_insert:minimal_area_helper([G1, G2]))
        end
    ).

strip_delta_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{2,6}, {0,2}]},

            ?assertEqual([G0, G1, G2],
                         rstar_insert:strip_delta([{0, G0}, {1, G1}, {2, G2}]))
        end
    ).

choose_subtree_leaf_test(_) ->
    ?_test(
        begin
            Root = #geometry{dimensions=2, mbr=[{1,3}, {0,4}], value=#leaf{}},
            Tree = #rtree{dimensions=2, root=Root},
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},

            % Select the root in the case of it being a leaf
            ?assertEqual(Root,
                         rstar_insert:choose_subtree(Tree, G0))
        end
    ).

choose_subtree_one_level_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,4}, {0,4}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{6,10}, {6,10}], value=#leaf{}},
            RootGeo = rstar_geometry:bounding_box([L1, L2]),
            Root = RootGeo#geometry{value=#node{children=[L1, L2]}},

            Tree = #rtree{dimensions=2, root=Root},
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},

            % Expect L1 to be selected, since it will cause no overlap
            % to occur, while L2 would overlap with L1.
            ?assertEqual(L1,
                         rstar_insert:choose_subtree(Tree, G0))
        end
    ).

choose_subtree_two_level_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,3}, {0,3}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,3}, {7,10}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{7,10}, {0,3}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{7,10}, {7,10}], value=#leaf{}},

            N1Geo = rstar_geometry:bounding_box([L1, L2]),
            N1 = N1Geo#geometry{value=#node{children=[L1, L2]}},

            N2Geo = rstar_geometry:bounding_box([L3, L4]),
            N2 = N2Geo#geometry{value=#node{children=[L3, L4]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2]}},

            Tree = #rtree{dimensions=2, root=Root},
            G0 = #geometry{dimensions=2, mbr=[{6,7}, {4,5}]},

            % Expect L3 to be selected, since it will need minimal
            % growth to contain it, while the rest will grow very large
            ?assertEqual(L3,
                         rstar_insert:choose_subtree(Tree, G0))
        end
    ).


