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
      fun find_leaf_not_exists_test/1
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

            ?assertEqual({found, [N, Root]}, rstar_delete:find_leaf(Root, L1))
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

