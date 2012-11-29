-module(rstar_insert).
-export([insert/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../include/rstar.hrl").

% ChooseSubtree:
% CS1: Set N to be the root
% CS2: If N is leaf, return N
% CS2b: If N points to leaves, select on
%   1) Minimal Overlap
%   2) Minimal Area Change
%   3) Smallest Area
% CS2c: IF N points to nodes, select on
%   1) Minimal Area Change
%   2) Smallest Area
choose_subtree(Node=#geometry{value=Value}, _, Path) when is_record(Value, leaf) -> [Node | Path];
choose_subtree(Node, Geo, Path) ->
    % Extract the children
    Children = Node#geometry.value#node.children,

    % Get the first child
    [FirstChild | _] = Children,

    % Check what kind of nodes the children are
    OptimialChildren = case FirstChild#geometry.value of
        #leaf{} ->
            MinOverlapDelta = minimal_overlap_delta(Geo, Children),
            MinAreaDelta = minimal_area_delta(Geo, MinOverlapDelta),
            minimal_area(MinAreaDelta);

        #node{} ->
            MinAreaDelta = minimal_area_delta(Geo, Children),
            minimal_area(MinAreaDelta)
    end,

    % Choose the first child
    [FirstOptimal | _] = OptimialChildren,

    % Recurse
    choose_subtree(FirstOptimal, Geo, [Node | Path]).


% Computes the overlap between a given geometry
% and a list of other geometries
overlap(Geo, OtherGeo) ->
    overlap_r(Geo, OtherGeo, 0).

% Do not computer overlap with original node
overlap_r(Geo, [Geo | Other], Sum) ->
    overlap_r(Geo, Other, Sum);

overlap_r(Geo, [G | Other], Sum) ->
    Intersect = rstar_geometry:intersect(Geo, G),
    case Intersect of
        undefined -> overlap_r(Geo, Other, Sum);
        _ ->
            Area = rstar_geometry:area(Intersect),
            overlap_r(Geo, Other, Sum + Area)
    end;

% Return the overlap when no more points remain
overlap_r(_, [], Sum) -> Sum.


% Returns the list of geometry objects
% that will have minimal change in overlap given the
% addition of a new geometry
minimal_overlap_delta(_Geo, L=[_X]) -> L;
minimal_overlap_delta(Geo, OtherGeo) ->
    strip_delta(minimal_overlap_delta_helper(Geo, OtherGeo)).

minimal_overlap_delta_helper(Geo, OtherGeo) ->
    Overlap = lists:map(fun (G) ->
        % Get a geometry that encompases this point
        Union = rstar_geometry:bounding_box([Geo, G]),

        % Compute the change in overlap
        Delta = overlap(Union, lists:delete(G, OtherGeo)) - overlap(G, OtherGeo),

        % Make a tuple with the delta and the Geo
        {Delta, G}

    end, OtherGeo),

    % Sort on the delta
    SortedOverlap = lists:keysort(1, Overlap),

    % Grab the head element
    [{FirstDelta, _FirstGeo} | _Tail] = SortedOverlap,

    % Filter the list to only those with equal overlap
    lists:takewhile(fun ({Delta, _}) ->
        Delta == FirstDelta
    end, SortedOverlap).


% Returns the list of geometry objects
% that will have minimal change in area given the
% addition of a geometryobject
minimal_area_delta(_Geo, L=[_X]) -> L;
minimal_area_delta(Geo, OtherGeo) ->
    strip_delta(minimal_area_delta_helper(Geo, OtherGeo)).

minimal_area_delta_helper(Geo, OtherGeo) ->
    Areas = lists:map(fun (G) ->
        % Get a geometry that encompases this point
        Union = rstar_geometry:bounding_box([Geo, G]),

        % Compute the change in overlap
        Delta = rstar_geometry:area(Union) - rstar_geometry:area(G),

        % Make a tuple with the delta and the Geo
        {Delta, G}

    end, OtherGeo),

    % Sort on the delta
    SortedArea = lists:keysort(1, Areas),

    % Grab the head element
    [{FirstDelta, _FirstGeo} | _Tail] = SortedArea,

    % Filter the list to only those with equal area delta
    lists:takewhile(fun ({Delta, _}) ->
        Delta == FirstDelta
    end, SortedArea).


% Returns the list of geometry objects
% that will have minimal area
minimal_area(L=[_X]) -> L;
minimal_area(OtherGeo) ->
    strip_delta(minimal_area_helper(OtherGeo)).

minimal_area_helper(OtherGeo) ->
    Areas = lists:map(fun (G) ->
        % Compute the area
        Area = rstar_geometry:area(G),

        % Make a tuple with the area and the Geo
        {Area, G}

    end, OtherGeo),

    % Sort on the delta
    SortedArea = lists:keysort(1, Areas),

    % Grab the head element
    [{FirstArea, _FirstGeo} | _Tail] = SortedArea,

    % Filter the list to only those with equal area delta
    lists:takewhile(fun ({Area, _}) ->
        Area == FirstArea
    end, SortedArea).


% Helper method that is used to strip the delta
% or area associated with the minimal_* functions.
strip_delta(Geo) -> [G || {_Delta, G} <- Geo].


% Split:
% S1: Invoke ChooseSplitAxis to determine axis to split on
% S2: Invoke ChooseSplitIndex to split into 2 groups along axis
% S3: Distribute into 2 groups
split(Params, Node) ->
    Axis = choose_split_axis(Params, Node),
    {GroupA, GroupB} = choose_split_index(Params, Node, Axis),

    % Create a bounding box for the distributions
    G1Geo = rstar_geometry:bounding_box(GroupA),
    G2Geo = rstar_geometry:bounding_box(GroupB),

    % Assign the children. The new nodes are a leaf type
    % only if we are splitting a leaf, otherwise they are
    % interior nodes.
    case Node#geometry.value of
        #leaf{} ->
            G1 = G1Geo#geometry{value=#leaf{entries=GroupA}},
            G2 = G2Geo#geometry{value=#leaf{entries=GroupB}};

        #node{} ->
            G1 = G1Geo#geometry{value=#node{children=GroupA}},
            G2 = G2Geo#geometry{value=#node{children=GroupB}}
    end,

    % Return the two new nodes
    {G1, G2}.


% ChooseSplitAxis
% CSA1: For each axis: Sort by lower, upper values of axes. Generate (M - 2*m + 2) distributions.
% Compute S, Sum of Margin values for all distributions.
% CSA2: Choose Axis with minimum S
choose_split_axis(Params, Node) ->
    N = Node#geometry.dimensions,
    Scored = [{axis_split_score(Params, Node, Axis), Axis} || Axis <- lists:seq(1, N)],
    Sorted = lists:keysort(1, Scored),
    [{_, MinAxis} | _] = Sorted,
    MinAxis.


% Computes the Score S for spliting on a given axis
axis_split_score(Params, Node, Axis) ->
    % Compute the distributions
    Distributions = axis_distributions(Params, Node, Axis),

    % Compute the margin sum
    lists:foldl(fun({GroupA, GroupB}, Sum) ->
        BoundA = rstar_geometry:bounding_box(GroupA),
        BoundB = rstar_geometry:bounding_box(GroupB),

        MarginA = rstar_geometry:margin(BoundA),
        MarginB = rstar_geometry:margin(BoundB),

        Sum + MarginA + MarginB

    end, 0, Distributions).


% Returns a list of all the possible distributions
% along the given axis
axis_distributions(Params, Node, Axis) ->
    % Ignore the record type, get the children
    {_, Children} = Node#geometry.value,

    % Sort the children on the appropriate axis
    Sorted = lists:sort(fun (A, B) ->
        {MinA, MaxA} = lists:nth(Axis, A#geometry.mbr),
        {MinB, MaxB} = lists:nth(Axis, B#geometry.mbr),

        % Sort on the lower and then upper part of the axis
        if
            MinA < MinB -> true;
            MinA == MinB -> MaxA =< MaxB;
            true -> false
        end
    end, Children),

    % Extract the tree parameters
    Min = Params#rt_params.min,
    Max = Params#rt_params.max,

    % Build the distribution
    [lists:split(K, Sorted) || K <- lists:seq(Min, Max - Min + 1)].


% ChooseSplitIndex
% CSI1: Along choosen axis, choose the distribution with minimum overlap value, resolve tie with minimum area value
choose_split_index(Params, Node, Axis) ->
    % Compute the distributions
    Distributions = axis_distributions(Params, Node, Axis),

    % Compute the overlap and area scores
    Scored = lists:map(fun(D={GroupA, GroupB}) ->
        BoundA = rstar_geometry:bounding_box(GroupA),
        BoundB = rstar_geometry:bounding_box(GroupB),
        Overlap = overlap(BoundA, [BoundB]),
        Area = rstar_geometry:area(BoundA) + rstar_geometry:area(BoundB),

        % Store with the overlap and area
        {Overlap, Area, D}

    end, Distributions),

    % Sort to get the lowest score. This will first sort on overlap,
    % and then on area
    Sorted = lists:sort(Scored),

    % Returns the best distribution
    [{_, _, Distrib} | _] = Sorted,
    Distrib.


% InsertData:
% I1: Invoke Insert with the leaf level as a param to add new geometry
insert(Params, Root, Geo) ->
    % Do an internal insert allowing for reinsertion
    {Root1, ReInsert} = insert_internal(Params, true, Root, Geo),

    % Re-insert without allowing for more reinsertion
    lists:foldl(fun(R, TmpRoot) ->
        {TmpRoot1, []} = insert_internal(Params, false, TmpRoot, R),
        TmpRoot1
    end, Root1, ReInsert).


% Insert:
% I1: Invoke ChooseSubtree with level as param to find node N for E
insert_internal(Params, AllowReinsert, Root, Geo) ->
    % Determine the traversal path, from root to leaf
    Path = choose_subtree(Root, Geo, []),
    RootToLeaf = lists:reverse(Path),

    % Perform a recursive insert and tree repair, since all
    % nodes on the path need to be adjusted
    {NewRoot, ReInsert} = insert_recursive(Params, AllowReinsert, 0, RootToLeaf, Geo),

    % Check if the root was split and needs to be reconstructed
    ReturnRoot = case NewRoot of
        {R1, R2} ->
            Children = [R1, R2],
            RootGeo = rstar_geometry:bounding_box(Children),
            RootGeo#geometry{value=#node{children=Children}};

        R -> R
    end,

    % Return the new root along with any nodes to be re-inserted
    {ReturnRoot, ReInsert}.


% Insert:
% I2: If N has < M entries, insert E. If N has M entries, invoke OverflowTreatment with the level of N as param.
% I3: If OverflowTreatment is called and a split performed, propogate OverflowTreatment upward. If OFT cause split of root, create new root.
insert_recursive(Params, AllowReinsert, Level, [Leaf], Geo) ->
    % Add the entry to the leaf
    NewEntries = [Geo | Leaf#geometry.value#leaf.entries],
    NewGeo = rstar_geometry:bounding_box(NewEntries),
    NewLeaf = NewGeo#geometry{value=#leaf{entries=NewEntries}},

    if
        % Check if there is space for this entry
        length(NewEntries) =< Params#rt_params.max ->
            {NewLeaf, []};

        % Overflow case, check for split or re-insert
        (Level == 0) or (not AllowReinsert) ->
            {N1, N2} = split(Params, NewLeaf),
            {{N1, N2}, []};

        % Allow reinsert in the special case
        true ->
            ReInserted = reinsert(Params, NewLeaf),
            ReducedChild = NewEntries -- ReInserted,
            ReducedGeo = rstar_geometry:bounding_box(ReducedChild),
            ReducedNode = ReducedGeo#geometry{value=#leaf{entries=ReducedChild}},
            {ReducedNode, ReInserted}
    end;

% I4: Adjust all the MBR in the insertion path
insert_recursive(Params, AllowReinsert, Level, [Parent | Tail=[Child| _]], Geo) ->
    {NewChild, ReInsert} = insert_recursive(Params, AllowReinsert, Level + 1, Tail, Geo),

    % Get the new children
    {_, Children} = Parent#geometry.value,
    NewChildren = case NewChild of
        {N1, N2} -> [N1, N2 | lists:delete(Child, Children)];
        NewNode ->  [NewNode | lists:delete(Child, Children)]
    end,

    % Update the bounding geometry and propogate
    NewGeo = rstar_geometry:bounding_box(NewChildren),
    NewParent = NewGeo#geometry{value=#node{children=NewChildren}},

    % Check for the split case
    AdjustedParent = case length(NewChildren) of
        % OFT: It is possible to handle overflow here with a reinsert or split,
        % however we only perform a split for simplicity
        L when L > Params#rt_params.max -> split(Params, NewParent);
        _ -> NewParent
    end,

    % Return the reconstructed level
    {AdjustedParent, ReInsert}.


% ReInsert:
% RI1: For all M+1 entries of N, compute distance from center of N to center of entry
% RI2: Sort the entries in decreasing order of distance
% RI3: Remove the first P entries from N and adjust MBR of N
% RI4: Invoke Insert with the removed entries (p= 30% of M), starting with farthest or closest
reinsert(Params, Node) ->
    % Get the center
    Center = rstar_geometry:center(Node),

    % Compute the distances
    {_, Children} = Node#geometry.value,
    Distances = lists:map(fun(G) ->
        {rstar_geometry:distance(Center, rstar_geometry:center(G)), G}
    end, Children),

    % Sort on distance
    Sorted = lists:sort(Distances),

    % Get the the number P of nodes to reinsert
    P = Params#rt_params.reinsert,

    % Determine what the nth tail would be
    LastP = Params#rt_params.max - P + 1,

    % Return the LastP nodes
    [N || {_Distance, N} <- lists:nthtail(LastP, Sorted)].


