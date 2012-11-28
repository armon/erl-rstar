-module(rstar_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-import(rstar, [new/1, insert/2, delete/2, search_within/2,
                search_nearest/3, search_around/3]).
-import(rstar_geometry, [origin/1, point2d/3]).

-include("../include/rstar.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun new_bad_dimension/1,
      fun new_valid/1,
      fun search_within_test/1,
      fun delete_test/1,
      fun search_nearest_test/1,
      fun search_around_test/1
     ]}.

setup() -> ok.
cleanup(_) -> ok.

new_bad_dimension(_) ->
    ?_test(
        begin
            ?assertEqual({error, badarg}, new(0)),
            ?assertEqual({error, badarg}, new(-1))
        end
    ).

new_valid(_) ->
    ?_test(
        begin
            Root1 = origin(2),
            DefParams = #rt_params{},
            Root2 = Root1#geometry{value=#leaf{}},
            Tree = #rtree{dimensions=2, params=DefParams,root=Root2},
            ?assertEqual(Tree, new(2))
        end
    ).

search_within_test(_) ->
    ?_test(
        begin
            T1 = new(2),
            P1 = point2d(1, 1, undefined),
            P2 = point2d(3, 3, undefined),
            P3 = point2d(7, 7, undefined),
            P4 = point2d(10, 10, undefine),

            T2 = insert(T1, P1),
            T3 = insert(T2, P2),
            T4 = insert(T3, P3),
            T5 = insert(T4, P4),

            G1 = #geometry{dimensions=2, mbr=[{-20, 20}, {-20, 20}]},
            ?assertEqual([P1, P2, P3, P4], search_within(T5, G1)),

            G2 = #geometry{dimensions=2, mbr=[{2, 20}, {-20, 20}]},
            ?assertEqual([P2, P3, P4], search_within(T5, G2))
        end
    ).

delete_test(_) ->
    ?_test(
        begin
            T1 = new(2),
            P1 = point2d(1, 1, undefined),
            P2 = point2d(3, 3, undefined),
            P3 = point2d(7, 7, undefined),
            P4 = point2d(10, 10, undefine),

            T2 = insert(T1, P1),
            T3 = insert(T2, P2),
            T4 = insert(T3, P3),
            T5 = insert(T4, P4),

            T6 = delete(T5, P1),
            T7 = delete(T6, P2),
            T8 = delete(T7, P3),
            T9 = delete(T8, P4),

            G1 = #geometry{dimensions=2, mbr=[{-20, 20}, {-20, 20}]},
            ?assertEqual([], search_within(T9, G1))
        end
    ).

search_nearest_test(_) ->
    ?_test(
        begin
            T1 = new(2),
            P1 = point2d(1, 1, undefined),
            P2 = point2d(3, 3, undefined),
            P3 = point2d(7, 7, undefined),
            P4 = point2d(10, 10, undefine),

            T2 = insert(T1, P1),
            T3 = insert(T2, P2),
            T4 = insert(T3, P3),
            T5 = insert(T4, P4),

            G1 = point2d(6, 6, undefined),
            ?assertEqual([P3, P2, P4, P1], search_nearest(T5, G1, 4)),
            ?assertEqual([P3, P2, P4], search_nearest(T5, G1, 3)),
            ?assertEqual([P3, P2], search_nearest(T5, G1, 2)),
            ?assertEqual([P3], search_nearest(T5, G1, 1))
        end
    ).

search_around_test(_) ->
    ?_test(
        begin
            T1 = new(2),
            P1 = point2d(1, 1, undefined),
            P2 = point2d(3, 3, undefined),
            P3 = point2d(7, 7, undefined),
            P4 = point2d(10, 10, undefine),

            T2 = insert(T1, P1),
            T3 = insert(T2, P2),
            T4 = insert(T3, P3),
            T5 = insert(T4, P4),

            G1 = point2d(6, 6, undefined),
            ?assertEqual([P1, P2, P3, P4], search_around(T5, G1, 10)),
            ?assertEqual([P2, P3], search_around(T5, G1, 5)),
            ?assertEqual([P3], search_around(T5, G1, 2)),
            ?assertEqual([], search_around(T5, G1, 1))
        end
    ).

