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
% CS2b: If N points to leaves, select on 1) Minimal Overlap 2) Minimal Area Change 3) Smallest Area
% CS2c: IF N points to nodes, select on 1) Minimal Area Change 2) Smallest Area
choose_subtree(Tree, Geo) ->
    choose_subtree(Tree, Tree#rtree.root, Geo).


choose_subtree(_, Node, _) ->
    case Node#geometry.value of
        #leaf{} -> Node;
        #node{children=[FirstChild | _Other]} ->
            % Check what kind of nodes the children are
            case FirstChild#geometry.value of
                #leaf{} -> ok;
                #node{} -> ok
            end
    end.


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
% that will have minimal overlap with the give
% geometry ovject
minimal_overlap_delta(Geo, OtherGeo) ->
    SortedOverlap = lists:map(fun (G) ->
        % Get a geometry that encompases this point
        Union = rstar_geometry:bounding_box([Geo, G]),

        % Compute the change in overlap
        Delta = overlap(Union, lists:delete(G, OtherGeo)) - overlap(G, OtherGeo),

        % Make a tuple with the delta and the Geo
        {Delta, G}

    end, OtherGeo),

    % Grab the head element
    [{FirstDelta, _FirstGeo} | _Tail] = SortedOverlap,

    % Filter the list to only those with equal overlap
    lists:takewhile(fun ({Delta, _}) ->
        Delta == FirstDelta
    end, SortedOverlap).


% Split:
% S1: Invoke ChooseSplitAxis to determine axis to split on
% S2: Invoke ChooseSplitIndex to split into 2 groups along axis
% S3: Distribute into 2 groups

% ChooseSplitAxis
% CSA1: For each axis: Sort by lower, upper values of axes. Generate (M - 2*m + 2) distributions. Compute S, Sum of Margin values for all distributions.
% CSA2: Choose Axis with minimum S

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

