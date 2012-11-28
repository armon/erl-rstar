-module(rstar_search).
-export([search_within/2, search_near/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../include/rstar.hrl").

% Searches the tree for any geometries within or intersecting
% with the given geometry
search_within(Root, Geo) -> search_within(Root, Geo, []).

search_within(#geometry{value=Value}, Geo, Results) when is_record(Value, leaf) ->
    lists:foldl(fun(Child, Res) ->
        case rstar_geometry:intersect(Child, Geo) of
            undefined -> Res;

            % Add the child to the result set
            _ -> [Child | Res]
        end
    end, Results, Value#leaf.entries);

search_within(Node, Geo, Results) ->
    lists:foldl(fun(Child, Res) ->
        case rstar_geometry:intersect(Child, Geo) of
            undefined -> Res;

            % Recurse into the matching node
            _ -> search_within(Child, Geo, Res)
        end
    end, Results, Node#geometry.value#node.children).


% Returns the K nearest points to the given Geometry
search_near(Node, Geo, K) ->
    Results = search_near_recursive(Node, Geo, K, gb_sets:new()),
    [R || {_, R} <- gb_sets:to_list(Results)].

search_near_recursive(#geometry{value=Value}, Geo, K, Results) when is_record(Value, leaf) ->
    lists:foldl(fun(C, Res) ->
        % Compute the center the distance to the target geometry
        Distance = rstar_geometry:min_dist(Geo, C),

        case gb_sets:size(Res) of
            % Handle the case where we have less than K matches
            L when L < K -> gb_sets:add({Distance, C}, Res);

            % Handle when we have K matches already
            _ ->
                {MaxDist, _} = MaxNN = gb_sets:largest(Res),
                if
                    MaxDist > Distance -> gb_sets:add({Distance, C}, gb_sets:delete(MaxNN, Res));
                    true -> Res
                end
        end
    end, Results, Value#leaf.entries);

search_near_recursive(Node, Geo, K, Results) ->
    % Create an Active Branch List based on our children sorted by their
    % minimum distance from the query region
    Children = Node#geometry.value#node.children,
    Sorted = lists:sort([{rstar_geometry:min_dist(Geo, C), C} || C <- Children]),
    ActiveBranchList = [B || {_, B} <- Sorted],

    % Prune the children and iterate over the branches
    Pruned = prune_branches(Geo, K, ActiveBranchList, Results),
    search_near_branches(Pruned, Geo, K, Results).


% Helper to iterate through the Active Branch List gathering results
% Return the result set when there are no further branches
search_near_branches([], _, _, Results) -> Results;

% Check each branch and prune after updating the results
search_near_branches([Branch | ABL], Geo, K, Results) ->
    NewResults = search_near_recursive(Branch, Geo, K, Results),
    Pruned = prune_branches(Geo, K, ABL, NewResults),
    search_near_branches(Pruned, Geo, K, NewResults).


% Prunes the search branches based on the existing results
prune_branches(Geo, K, Branches, Results) ->
    case gb_sets:size(Results) of
        % Do not prune if we don't have K neighbors yet
        L when L < K -> Branches;
        _ ->
            {MaxDist, _} = gb_sets:largest(Results),
            lists:filter(fun(B) -> rstar_geometry:min_dist(Geo, B) =< MaxDist end, Branches)
    end.

