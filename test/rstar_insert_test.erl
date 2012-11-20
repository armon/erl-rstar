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
      fun some_overlap_3d_test/1
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


