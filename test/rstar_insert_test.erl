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
      fun strip_delta_test/1
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
                         rstar_insert:minimal_overlap_delta(G0, [G1, G2]))
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
                         rstar_insert:minimal_overlap_delta(G0, [G1, G2, G3]))
        end
    ).

minimal_area_delta_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,4}]},
            ?assertEqual([{4, G1}],
                         rstar_insert:minimal_area_delta(G0, [G1, G2]))
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
                         rstar_insert:minimal_area_delta(G0, [G1, G2, G3]))
        end
    ).


minimal_area_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{4,6}, {0,2}]},
            ?assertEqual([{4, G2}],
                         rstar_insert:minimal_area(G0, [G1, G2]))
        end
    ).

minimal_area_tie_test(_) ->
    ?_test(
        begin
            G0 = #geometry{dimensions=2, mbr=[{1,3}, {0,4}]},
            G1 = #geometry{dimensions=2, mbr=[{0,2}, {0,4}]},
            G2 = #geometry{dimensions=2, mbr=[{2,6}, {0,2}]},

            ?assertEqual([{8, G1}, {8, G2}],
                         rstar_insert:minimal_area(G0, [G1, G2]))
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


