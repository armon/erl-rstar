-module(rstar_delete).
-export([delete/3]).

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
-spec find_leaf(#geometry{}, #geometry{}) -> not_found | {found, [#geometry{}]}.
find_leaf(Node, Geo) -> find_leaf(Node, Geo, []).

find_leaf(Node=#geometry{value=Value}, Geo, Path) when is_record(Value, leaf) ->
    % Search the children
    Children = Node#geometry.value#leaf.entries,
    case lists:member(Geo, Children) of
        true -> {found, lists:reverse([Node | Path])};
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


% Deletes the specified geometry from the tree
% Returns not_found or the new root
-spec delete(#rt_params{}, #geometry{}, #geometry{}) -> not_found | #geometry{}.
delete(Params, Root, Geo) ->
    % Find the leaf
    case find_leaf(Root, Geo) of
        not_found -> not_found;
        {found, Path} -> delete_internal(Params, Path, Geo)
    end.


delete_internal(Params, Path, Geo) ->
    % Recursively handle the delete
    [Root | _] = Path,
    {AdjRoot, ReInsert} = delete_recursive(Params, Root, Path, Geo),

    % Handle the case of the root with a single child
    AdjRoot1 = case AdjRoot#geometry.value of
        {node, [RootChild]} -> RootChild;
        _ -> AdjRoot
    end,

    % Re-insert to get the final root
    lists:foldl(fun(Insert, ReInsRoot) ->
        rstar_insert:insert(Params, ReInsRoot, Insert)
    end, AdjRoot1, ReInsert).


delete_recursive(Params, Root, [Leaf], Geo) ->
    Children = Leaf#geometry.value#leaf.entries,
    NewChildren = Children -- [Geo],
    case length(NewChildren) of
        % If we underflow the leaf, then we delete the leaf, and
        % re-insert all the children
        L when (Root =/= Leaf) and (L < Params#rt_params.min) -> {undefined, NewChildren};

        % If we are the root, and we delete the last item, we need to
        % deal with the empty leaf case
        0 ->
            LeafGeo = rstar_geometry:origin(Root#geometry.dimensions),
            NewLeaf = LeafGeo#geometry{value=#leaf{entries=[]}},
            {NewLeaf, []};

        % If we do not underflow, adjust the bounds
        _ ->
            LeafGeo = rstar_geometry:bounding_box(NewChildren),
            NewLeaf = LeafGeo#geometry{value=#leaf{entries=NewChildren}},
            {NewLeaf, []}
    end;

delete_recursive(Params, Root, [Parent | Tail=[Child| _]], Geo) ->
    % Recurse first
    {NewChild, ReInsert} = delete_recursive(Params, Root, Tail, Geo),

    % Update our children
    {_, Children} = Parent#geometry.value,
    NewChildren = case NewChild of
        undefined -> lists:delete(Child, Children);
        NewNode ->  [NewNode | lists:delete(Child, Children)]
    end,

    % Check for the underflow case
    case length(NewChildren) of
        % It is possible to handle underflow here
        % by re-inserting inner nodes, but simpler to just
        % re-insert the records
        L when (Root =/= Parent) and (L < Params#rt_params.min) ->
            SubRecords = collect_records(NewChildren),
            {undefined, SubRecords ++ ReInsert};

        _ ->
            % Update the bounding geometry and propogate
            NewGeo = rstar_geometry:bounding_box(NewChildren),
            NewParent = NewGeo#geometry{value=#node{children=NewChildren}},
            {NewParent, ReInsert}
    end.



% Peforms a pre-order traversal of the subtree and returns
% all the records from the leaf nodes. Results as if leaves were
% visited in post order traversal
collect_records(Nodes) when is_list(Nodes) ->
    Records = lists:foldl(fun(N, Recs) ->
        [collect_records(N, []) | Recs]
    end, [], Nodes),
    lists:flatten(Records);

collect_records(Node) ->
    Records = collect_records(Node, []),
    lists:append(Records).

collect_records(Node=#geometry{value=Value}, Records) when is_record(Value, leaf) ->
   Children = Node#geometry.value#leaf.entries,
   [Children | Records];

collect_records(Node, Records) ->
   Children = Node#geometry.value#node.children,
    lists:foldl(fun(Child, Rec) ->
        collect_records(Child, Rec)
    end, Records, Children).

