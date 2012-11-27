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
      fun choose_subtree_two_level_test/1,
      fun axis_distribution_test/1,
      fun axis_split_score_test/1,
      fun choose_split_axis_test/1,
      fun choose_split_index_test/1,
      fun split_test/1,
      fun reinsert_test/1,
      fun insert_recursive_no_overflow_test/1,
      fun insert_recursive_force_split_test/1,
      fun insert_recursive_force_reinsert_test/1,
      fun insert_recursive_child_insert_test/1,
      fun insert_recursive_child_split_test/1,
      fun insert_recursive_parent_split_test/1,
      fun insert_internal_no_split_test/1,
      fun insert_internal_root_split_test/1,
      fun insert_no_reinsert_test/1,
      fun insert_reinsert_test/1
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
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},

            % Select the root in the case of it being a leaf
            ?assertEqual([Root],
                         rstar_insert:choose_subtree(Root, G0, []))
        end
    ).

choose_subtree_one_level_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,4}, {0,4}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{6,10}, {6,10}], value=#leaf{}},
            RootGeo = rstar_geometry:bounding_box([L1, L2]),
            Root = RootGeo#geometry{value=#node{children=[L1, L2]}},

            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},

            % Expect L1 to be selected, since it will cause no overlap
            % to occur, while L2 would overlap with L1.
            ?assertEqual([L1, Root],
                         rstar_insert:choose_subtree(Root, G0, []))
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

            G0 = #geometry{dimensions=2, mbr=[{6,7}, {4,5}]},

            % Expect L3 to be selected, since it will need minimal
            % growth to contain it, while the rest will grow very large
            ?assertEqual([L3, N2, Root],
                         rstar_insert:choose_subtree(Root, G0, []))
        end
    ).

axis_distribution_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,1}, {6,10}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{1,4}, {0,4}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{6,5}, {6,10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{3,9}, {6,10}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L1, L2, L3, L4, L5]),
            N = NGeo#geometry{value=#node{children=[L1, L2, L3, L4, L5]}},


            % Sorted = [L2, L1, L3, L5, L4],
            Params1 = #rt_params{min=1, max=4},
            Expected1 = [
                {[L2], [L1, L3, L5, L4]},
                {[L2, L1], [L3, L5, L4]},
                {[L2, L1, L3], [L5, L4]},
                {[L2, L1, L3, L5], [L4]}
            ],

            ?assertEqual(Expected1,
                         rstar_insert:axis_distributions(Params1, N, 1)),

            % Sorted = [L2, L1, L3, L5, L4],
            Params2 = #rt_params{min=2, max=4},
            Expected2 = [
                {[L2, L1], [L3, L5, L4]},
                {[L2, L1, L3], [L5, L4]}
            ],

            ?assertEqual(Expected2,
                         rstar_insert:axis_distributions(Params2, N, 1))
        end
    ).

axis_split_score_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,2}, {0,10}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,1}, {0,10}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{1,4}, {0,10}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{6,8}, {0,10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{3,9}, {0,10}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L1, L2, L3, L4, L5]),
            N = NGeo#geometry{value=#node{children=[L1, L2, L3, L4, L5]}},


            % This is basically just very tedious to figure out.
            % Based on the Expected1 distribution above, manually compute
            % the margin sum as 242
            Params1 = #rt_params{min=1, max=4},
            ?assertEqual(242.0,
                         rstar_insert:axis_split_score(Params1, N, 1))
        end
    ).

choose_split_axis_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{3,4}, {3,6}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{4,5}, {3,6}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{5,6}, {3,6}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{4,7}, {8,10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{4,7}, {10,11}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L1, L2, L3, L4, L5]),
            N = NGeo#geometry{value=#node{children=[L1, L2, L3, L4, L5]}},


            % Difficult to explain, we are creating 3 modes with a bounding box
            % at {3,6} {3, 6}, and 2 at {4, 7} {8, 11}. A split on the X axis
            % does not cleanly divide the two groups, and thus there is a large
            % margin. A split on the Y axis creates 2 clusters, each as tightly
            % packed squares. We thus expect the Y axis to be selected.
            Params1 = #rt_params{min=2, max=4},
            ?assertEqual(2,
                         rstar_insert:choose_split_axis(Params1, N))
        end
    ).

choose_split_index_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{3,4}, {3,6}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{4,5}, {3,6}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{5,6}, {3,6}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{4,7}, {8,10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{4,7}, {10,11}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L5, L4, L3, L2, L1]),
            N = NGeo#geometry{value=#node{children=[L5, L4, L3, L2, L1]}},

            % We are creating 3 modes with a bounding box
            % at {3,6} {3, 6}, and 2 at {4, 7} {8, 11}. A split on the X axis
            % does not cleanly divide the two groups, and thus there is a large
            % margin. A split on the Y axis can create 2 clusters, each as tightly
            % packed squares. We thus expect the two non-overlaping squares to be selected.
            Params1 = #rt_params{min=2, max=4},

            Expected = {[L3, L2, L1], [L4, L5]},
            ?assertEqual(Expected,
                         rstar_insert:choose_split_index(Params1, N, 2))
        end
    ).

split_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{3,4}, {3,6}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{4,5}, {3,6}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{5,6}, {3,6}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{4,7}, {8,10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{4,7}, {10,11}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L5, L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L5, L4, L3, L2, L1]}},

            % We are creating 3 modes with a bounding box
            % at {3,6} {3, 6}, and 2 at {4, 7} {8, 11}. A split on the X axis
            % does not cleanly divide the two groups, and thus there is a large
            % margin. A split on the Y axis can create 2 clusters, each as tightly
            % packed squares. We thus expect the two non-overlaping squares to be selected.
            Params1 = #rt_params{min=2, max=4},

            % Create the expected explits
            GroupA = [L3, L2, L1],
            GroupB = [L4, L5],
            G1Geo = rstar_geometry:bounding_box(GroupA),
            G2Geo = rstar_geometry:bounding_box(GroupB),
            G1 = G1Geo#geometry{value=#leaf{entries=GroupA}},
            G2 = G2Geo#geometry{value=#leaf{entries=GroupB}},

            Expected = {G1, G2},
            ?assertEqual(Expected,
                         rstar_insert:split(Params1, N))
        end
    ).

reinsert_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{10, 10}, {10, 10}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-9, -9}, {-10, -10}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L5, L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L5, L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            Expected = [L4, L3],
            ?assertEqual(Expected, rstar_insert:reinsert(Params1, N))
        end
    ).

insert_recursive_no_overflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            NewGeo = rstar_geometry:bounding_box([G, L4, L3, L2, L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[G, L4, L3, L2, L1]}},

            Expected = {ExpectedNode, []},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, true, 10, [N], G))
        end
    ).

insert_recursive_force_split_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a split
            N1Geo = rstar_geometry:bounding_box([L4, L2]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[L4, L2]}},

            N2Geo = rstar_geometry:bounding_box([G, L3, L1]),
            ExpectN2 = N2Geo#geometry{value=#leaf{entries=[L1, L3, G]}},

            Expected = {{ExpectN1, ExpectN2}, []},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, false, 10, [N], G))
        end
    ).

insert_recursive_force_reinsert_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a split
            N1Geo = rstar_geometry:bounding_box([L1, L2, L3]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[L3, L2, L1]}},

            Expected = {ExpectN1, [L4, G]},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, true, 10, [N], G))
        end
    ).

insert_recursive_child_insert_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            NewGeo = rstar_geometry:bounding_box([G, L4, L3, L2, L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[G, L4, L3, L2, L1]}},
            ExpectedRoot = NewGeo#geometry{value=#node{children=[ExpectedNode]}},

            Expected = {ExpectedRoot, []},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, true, 10, [Root, N], G))
        end
    ).

insert_recursive_child_split_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a split
            N1Geo = rstar_geometry:bounding_box([L4, L2]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[L4, L2]}},

            N2Geo = rstar_geometry:bounding_box([G, L3, L1]),
            ExpectN2 = N2Geo#geometry{value=#leaf{entries=[L1, L3, G]}},

            % Root should have both children
            ExpectRootGeo = rstar_geometry:bounding_box([ExpectN1, ExpectN2]),
            ExpectRoot = ExpectRootGeo#geometry{value=#node{children=[ExpectN1, ExpectN2]}},

            Expected = {ExpectRoot, []},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, false, 10, [Root, N], G))
        end
    ).

insert_recursive_parent_split_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N, L2, L2, L2]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a split
            N1Geo = rstar_geometry:bounding_box([L4, L2]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[L4, L2]}},

            N2Geo = rstar_geometry:bounding_box([G, L3, L1]),
            ExpectN2 = N2Geo#geometry{value=#leaf{entries=[L1, L3, G]}},

            % Root should split
            ExpectRootGeo1 = rstar_geometry:bounding_box([ExpectN1, L2]),
            ExpectRoot1 = ExpectRootGeo1#geometry{value=#node{children=[ExpectN1, L2]}},

            ExpectRootGeo2 = rstar_geometry:bounding_box([ExpectN2, L2]),
            ExpectRoot2 = ExpectRootGeo2#geometry{value=#node{children=[L2, L2, ExpectN2]}},

            Expected = {{ExpectRoot1, ExpectRoot2}, []},
            ?assertEqual(Expected, rstar_insert:insert_recursive(Params1, false, 10, [Root, N], G))
        end
    ).


insert_internal_no_split_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            NewGeo = rstar_geometry:bounding_box([G, L4, L3, L2, L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[G, L4, L3, L2, L1]}},
            ExpectedRoot = NewGeo#geometry{value=#node{children=[ExpectedNode]}},

            Expected = {ExpectedRoot, []},
            ?assertEqual(Expected, rstar_insert:insert_internal(Params1, true, Root, G))
        end
    ).

insert_internal_root_split_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N, L2, L2, L2]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a split
            N1Geo = rstar_geometry:bounding_box([L4, L2]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[L4, L2]}},

            N2Geo = rstar_geometry:bounding_box([G, L3, L1]),
            ExpectN2 = N2Geo#geometry{value=#leaf{entries=[L1, L3, G]}},

            % Root should split
            ExpectRootGeo1 = rstar_geometry:bounding_box([ExpectN1, L2]),
            ExpectRoot1 = ExpectRootGeo1#geometry{value=#node{children=[ExpectN1, L2]}},

            ExpectRootGeo2 = rstar_geometry:bounding_box([ExpectN2, L2]),
            ExpectRoot2 = ExpectRootGeo2#geometry{value=#node{children=[L2, L2, ExpectN2]}},

            % Merged Root
            MergedGeo = rstar_geometry:bounding_box([ExpectRoot1, ExpectRoot2]),
            MergedRoot = MergedGeo#geometry{value=#node{children=[ExpectRoot1, ExpectRoot2]}},

            Expected = {MergedRoot, []},
            ?assertEqual(Expected, rstar_insert:insert_internal(Params1, false, Root, G))
        end
    ).

insert_no_reinsert_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            NewGeo = rstar_geometry:bounding_box([G, L4, L3, L2, L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[G, L4, L3, L2, L1]}},
            ExpectedRoot = NewGeo#geometry{value=#node{children=[ExpectedNode]}},

            ?assertEqual(ExpectedRoot, rstar_insert:insert(Params1, Root, G))
        end
    ).

insert_reinsert_test(_) ->
    ?_test(
        begin
            % Construct initial structure
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=4, reinsert=2},

            EmptyGeo = rstar_geometry:origin(2),
            EmptyN = EmptyGeo#geometry{value=#leaf{entries=[L2, L2]}},

            Root = NGeo#geometry{value=#node{children=[N, EmptyN]}},

            % New node
            G = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},

            % Expect a re-insert, rebalancing the nodes
            N1Geo = rstar_geometry:bounding_box([L1, L2, L3, G]),
            ExpectN1 = N1Geo#geometry{value=#leaf{entries=[G, L3, L2, L1]}},

            N2Geo = rstar_geometry:bounding_box([L2, L4]),
            ExpectN2 = N2Geo#geometry{value=#leaf{entries=[L4, L2, L2]}},

            ExpectRootGeo = rstar_geometry:bounding_box([N1Geo, N2Geo]),
            ExpectedRoot = ExpectRootGeo#geometry{value=#node{children=[ExpectN1, ExpectN2]}},

            ?assertEqual(ExpectedRoot, rstar_insert:insert(Params1, Root, G))
        end
    ).

