-module(rstar_delete).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../include/rstar.hrl").


% FindLeaf
% FL1: If T is not a leaf, check each entry F in T
% for overlap. For each entry, invoke FindLeaf until
% E is found or all entries have been checked.
% FL2: If T is a leaf, check each entry to see if it matches
% E. If E is found, return T
find_leaf(Node, Geo) -> find_leaf(Node, Geo, []).

find_leaf(Node=#geometry{value=Value}, Geo, Path) when is_record(Value, leaf) ->
    % Search the children
    Children = Node#geometry.value#leaf.entries,
    case lists:member(Geo, Children) of
        true -> {found, [Node | Path]};
        false -> not_found
    end;

find_leaf(Node, Geo, Path) ->
    % Extract the children
    Children = Node#geometry.value#node.children,

    % Check each child for overlap
    Overlapping = lists:foldl(fun(Child, Matching) ->
        case rstar_geometry:intersect(Geo, Child) of
            undefined -> Matching;
            _Overlap -> [Child | Matching]
        end
    end, [], Children),

    % Search each child
    find_leaf_helper(Overlapping, Geo, [Node | Path]).


% Iterates through a list of nodes invoking find_leaf
find_leaf_helper([], _, _) -> not_found;
find_leaf_helper([Node | Tail], Geo, Path) ->
    case find_leaf(Node, Geo, Path) of
        not_found -> find_leaf_helper(Tail, Geo, Path);
        X -> X
    end.


