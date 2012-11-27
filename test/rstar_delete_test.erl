-module(rstar_delete_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun find_leaf_exists_test/1,
      fun find_leaf_not_exists_test/1,
      fun collect_records_leaf_test/1,
      fun collect_records_one_level_test/1,
      fun collect_records_list_test/1,
      fun delete_recursive_leaf_no_undeflow_test/1,
      fun delete_recursive_leaf_root_underflow_test/1,
      fun delete_recursive_leaf_underflow_test/1,
      fun delete_recursive_child_underflow_test/1,
      fun delete_recursive_child_no_underflow_test/1,
      fun delete_recursive_parent_underflow_test/1,
      fun delete_recursive_parent_root_underflow_test/1,
      fun delete_internal_no_underflow_test/1,
      fun delete_internal_single_child_test/1,
      fun delete_internal_reinsert_test/1,
      fun delete_not_found_test/1,
      fun delete_empty_root_test/1,
      fun delete_found_test/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

find_leaf_exists_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},

            ?assertEqual({found, [Root, N]}, rstar_delete:find_leaf(Root, L1))
        end
    ).

find_leaf_not_exists_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Root = NGeo#geometry{value=#node{children=[N]}},

            G = #geometry{dimensions=2, mbr=[{0.5, 0.5}, {0.5, 0.5}], value=#leaf{}},

            ?assertEqual(not_found, rstar_delete:find_leaf(Root, G))
        end
    ).

collect_records_leaf_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            ?assertEqual([L4, L3, L2, L1], rstar_delete:collect_records(N))
        end
    ).

collect_records_one_level_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},

            N1Geo = rstar_geometry:bounding_box([L4, L3]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L3, L4]}},

            N2Geo = rstar_geometry:bounding_box([L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L1, L2]}},

            Root = N1Geo#geometry{value=#node{children=[N1, N2]}},
            ?assertEqual([L1, L2, L3, L4], rstar_delete:collect_records(Root))
        end
    ).

collect_records_list_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},

            N1Geo = rstar_geometry:bounding_box([L4, L3]),
            N1 = N1Geo#geometry{value=#leaf{entries=[L3, L4]}},

            N2Geo = rstar_geometry:bounding_box([L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L1, L2]}},

            Root = N1Geo#geometry{value=#node{children=[N1, N2]}},
            ?assertEqual([L1, L2, L3, L4, L1, L2, L3, L4],
                         rstar_delete:collect_records([Root, Root]))
        end
    ).

delete_recursive_leaf_no_undeflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            NewGeo = rstar_geometry:bounding_box([L3, L2, L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[L3, L2, L1]}},

            Expected = {ExpectedNode, []},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, N, [N], L4))
        end
    ).

delete_recursive_leaf_root_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            NewGeo = rstar_geometry:bounding_box([L1]),
            ExpectedNode = NewGeo#geometry{value=#leaf{entries=[L1]}},

            % Root cannot underflow
            Expected = {ExpectedNode, []},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, N, [N], L2))
        end
    ).

delete_recursive_leaf_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Should underflow
            Expected = {undefined, [L1]},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, L1, [N], L2))
        end
    ).

delete_recursive_child_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L2, L1]}},

            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            N3Geo = rstar_geometry:bounding_box([L4, L3, L1]),
            N3 = N3Geo#geometry{value=#leaf{entries=[L4, L3, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2, N3]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2, N3]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Create the expected root
            ExpectGeo = rstar_geometry:bounding_box([N2, N3]),
            ExpectRoot = ExpectGeo#geometry{value=#node{children=[N2, N3]}},

            % Should underflow child only
            Expected = {ExpectRoot, [L1]},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, Root, [Root, N1], L2))
        end
    ).

delete_recursive_child_no_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},

            NGeo = rstar_geometry:bounding_box([L3, L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L3, L2, L1]}},

            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            N3Geo = rstar_geometry:bounding_box([L4, L3, L1]),
            N3 = N3Geo#geometry{value=#leaf{entries=[L4, L3, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2, N3]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2, N3]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Create the expected root
            ExpectN1Geo = rstar_geometry:bounding_box([L3, L1]),
            ExpectN1 = ExpectN1Geo#geometry{value=#leaf{entries=[L3, L1]}},

            ExpectGeo = rstar_geometry:bounding_box([ExpectN1, N2, N3]),
            ExpectRoot = ExpectGeo#geometry{value=#node{children=[ExpectN1, N2, N3]}},

            % No underflow expected
            Expected = {ExpectRoot, []},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, Root, [Root, N1], L2))
        end
    ).

delete_recursive_parent_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L2, L1]}},

            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Should underflow child and parent
            Expected = {undefined, [L4, L3, L2, L1, L1]},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, L2, [Root, N1], L2))
        end
    ).

delete_recursive_parent_root_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L2, L1]}},

            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Create the expected root
            ExpectRoot = N2Geo#geometry{value=#node{children=[N2]}},

            % Cannot underflow root
            Expected = {ExpectRoot, [L1]},
            ?assertEqual(Expected, rstar_delete:delete_recursive(Params1, Root, [Root, N1], L2))
        end
    ).

delete_internal_no_underflow_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},

            NGeo = rstar_geometry:bounding_box([L3, L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L3, L2, L1]}},

            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            N3Geo = rstar_geometry:bounding_box([L4, L3, L1]),
            N3 = N3Geo#geometry{value=#leaf{entries=[L4, L3, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2, N3]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2, N3]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Create the expected root
            ExpectN1Geo = rstar_geometry:bounding_box([L3, L1]),
            ExpectN1 = ExpectN1Geo#geometry{value=#leaf{entries=[L3, L1]}},

            ExpectGeo = rstar_geometry:bounding_box([ExpectN1, N2, N3]),
            ExpectRoot = ExpectGeo#geometry{value=#node{children=[ExpectN1, N2, N3]}},

            % No underflow expected
            ?assertEqual(ExpectRoot, rstar_delete:delete_internal(Params1, [Root, N1], L2))
        end
    ).

delete_internal_single_child_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},

            NGeo = rstar_geometry:bounding_box([L2]),
            N1 = NGeo#geometry{value=#leaf{entries=[L2]}},

            N2Geo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            ?assertEqual(N2, rstar_delete:delete_internal(Params1, [Root, N1], L2))
        end
    ).

delete_internal_reinsert_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            L5 = #geometry{dimensions=2, mbr=[{3, 3}, {3, 3}], value=#leaf{}},
            L6 = #geometry{dimensions=2, mbr=[{4, 4}, {4, 4}], value=#leaf{}},

            NGeo = rstar_geometry:bounding_box([L2, L1]),
            N1 = NGeo#geometry{value=#leaf{entries=[L2, L1]}},

            N2Geo = rstar_geometry:bounding_box([L4, L3]),
            N2 = N2Geo#geometry{value=#leaf{entries=[L4, L3]}},

            N3Geo = rstar_geometry:bounding_box([L5, L6]),
            N3 = N3Geo#geometry{value=#leaf{entries=[L5, L6]}},

            RootGeo = rstar_geometry:bounding_box([N1, N2, N3]),
            Root = RootGeo#geometry{value=#node{children=[N1, N2, N3]}},

            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % We expect N1 will underflow, causing a re-insertion of L1
            % This should logically go in N2
            ExpectN2Geo = rstar_geometry:bounding_box([L4, L3, L1]),
            ExpectN2 = ExpectN2Geo#geometry{value=#leaf{entries=[L1, L4, L3]}},

            ExpectRootGeo = rstar_geometry:bounding_box([ExpectN2, N3]),
            ExpectRoot = ExpectRootGeo#geometry{value=#node{children=[ExpectN2, N3]}},

            ?assertEqual(ExpectRoot, rstar_delete:delete_internal(Params1, [Root, N1], L2))
        end
    ).

delete_not_found_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            G = #geometry{dimensions=2, mbr=[{0.5, 0.5}, {0.5, 0.5}], value=#leaf{}},

            ?assertEqual(not_found, rstar_delete:delete(Params1, N, G))
        end
    ).

delete_empty_root_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L1]),
            N = NGeo#geometry{value=#leaf{entries=[L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            % Delete only item
            Expected = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            ?assertEqual(Expected, rstar_delete:delete(Params1, N, L1))
        end
    ).

delete_found_test(_) ->
    ?_test(
        begin
            L1 = #geometry{dimensions=2, mbr=[{1,1}, {1,1}], value=#leaf{}},
            L2 = #geometry{dimensions=2, mbr=[{0,0}, {0,0}], value=#leaf{}},
            L3 = #geometry{dimensions=2, mbr=[{2, 2}, {2, 2}], value=#leaf{}},
            L4 = #geometry{dimensions=2, mbr=[{-1, -1}, {-1, -1}], value=#leaf{}},
            NGeo = rstar_geometry:bounding_box([L4, L3, L2, L1]),
            N = NGeo#geometry{value=#leaf{entries=[L4, L3, L2, L1]}},
            Params1 = #rt_params{min=2, max=5, reinsert=2},

            ExpectGeo = rstar_geometry:bounding_box([L4, L3, L2]),
            ExpectRoot = ExpectGeo#geometry{value=#leaf{entries=[L4, L3, L2]}},

            ?assertEqual(ExpectRoot, rstar_delete:delete(Params1, N, L1))
        end
    ).

