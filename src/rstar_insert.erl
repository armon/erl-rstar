-module(rstar_insert).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../include/rstar.hrl").

% R-Tree:
% Insert:
% I1: Invoke ChooseLeaf to select leaf L to place E
% I2: If L has room for E, install E. Else, invoke SplitNode to get L and LL.
% I3: Invoke AdjustTree on L, providing LL on a split
% I4: If root split, create new root
%
% ChooseLeaf:
% CL1: Set N to be the root
% CL2: If N is leaf, return N
% CL3: If N not leaf, F in N, F requires smallest enlargement to contain E
% Resolve tie using smallest area
% CL4: Set N to be F, repeat until leaf
%
% AdjustTree:
% AT1: Set N=L, set NN=LL if applicable
% AT2: If N is root, stop
% AT3: P = parent(N), Adjust covering rectangle
% AT4: If NN, add to P if room. Otherwise SplitNode, to get P and PP.
% AT5: Set N=P, NN=PP repeat from AT2

% R*-tree:
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
choose_subtree(Tree, Geo) ->
    choose_subtree(Tree, Tree#rtree.root, Geo).

choose_subtree(_, Node=#geometry{value=Value}, _) when is_record(Value, leaf) -> Node;
choose_subtree(Tree, Node, Geo) ->
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
    choose_subtree(Tree, FirstOptimal, Geo).


% Computes the overlap between a given geometry
% and a list of other geometries
overlap(Geo, OtherGeo) ->
    lists:foldl(fun(G, Sum) ->
        % Do not compute the intersection of ourself
        Intersect = case G of
            Geo -> undefined;
            _ -> rstar_geometry:intersect(Geo, G)
        end,

        % Compute the area if we have one
        Area = case Intersect of
            undefined -> 0;
            _ -> rstar_geometry:area(Intersect)
        end,

        % Grow the accumulator by the overlap area
        Sum + Area
    end, 0, OtherGeo).


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

% ChooseSplitAxis
% CSA1: For each axis: Sort by lower, upper values of axes. Generate (M - 2*m + 2) distributions. Compute S, Sum of Margin values for all distributions.
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
    Distributions = axis_distributions(Params,Node, Axis),

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

% InsertData:
% I1: Invoke Insert with the leaf level as a param to add new geometry

% Insert:
% I1: Invoke ChooseSubtree with level as param to find node N for E
% I2: If N has < M entries, insert E. If N has M entries, invoke OverflowTreatment with the level of N as param.
% I3: If OverflowTreatment is called and a split performed, propogate OverflowTreatment upward. If OFT cause split of root, create new root.
% I4: Adjust all the MBR in the insertion path

% OverflowTreatment
% OFT1: If level is not root, and this is first call of OFT at the given level invoke ReInsert, else invoke Split.

% ReInsert:
% RI1: For all M+1 entries of N, compute distance from center of N to center of entry
% RI2: Sort the entries in decreasing order of distance
% RI3: Remove the first P entries from N and adjust MBR of N
% RI4: Invoke Insert with the removed entries (p= 30% of M), starting with farthest or closest

num_entries(Node) -> length(Node#geometry.value).

