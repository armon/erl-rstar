-module(rstar_geometry).
-export([new/3, origin/1, point2d/3, point3d/4, bounding_box/1,
         area/1, intersect/2, num_edges/1, margin/1, center/1, distance/2]).

-include("../include/rstar.hrl").

% Creates a new geometry record
-spec new(integer(), list({float(), float()}), any()) -> {error, badarg} | #geometry{}.
new(Dimensions, _, _) when Dimensions < 1 -> {error, badarg};
new(Dimensions, MBR, Value) ->
    case valid_axes(MBR, 0) of
        {ok, Dimensions} ->
            #geometry{dimensions=Dimensions, mbr=MBR, value=Value};
        _ -> {error, badarg}
    end.


% Creates a point at the origin with the proper dimensionality
-spec origin(integer()) -> #geometry{} | {error, badarg}.
origin(Dimensions) when Dimensions < 1 -> {error, badarg};
origin(Dimensions) ->
    MBR = [{0, 0} || _D <- lists:seq(1, Dimensions)],
    #geometry{dimensions=Dimensions, mbr=MBR}.


% Helper to create a 2D point
-spec point2d(float(), float(), any()) -> #geometry{}.
point2d(X, Y, Value) -> new(2, [{X, X}, {Y, Y}], Value).


% Helper to create a 3D point
-spec point3d(float(), float(), float(), any()) -> #geometry{}.
point3d(X, Y, Z, Value) -> new(3, [{X, X}, {Y, Y}, {Z, Z}], Value).


% Returns a new geometry which is a bounding box of
% the given geometries
-spec bounding_box([#geometry{}]) -> #geometry{}.
bounding_box([]) -> {error, badarg};
bounding_box([First | MoreGeo]) ->
    % Fold over each geometry and extend the MBR
    BindingMBR = lists:foldl(fun (Geo, Bounding) ->
        % Zip the MBR axes of the current Geometry with
        % that of the Bounding axes
        lists:zipwith(fun ({MinA, MaxA}, {MinB, MaxB}) ->
            {min(MinA, MinB), max(MaxA, MaxB)}
        end, Geo#geometry.mbr, Bounding)
    end, First#geometry.mbr, MoreGeo),

    % Create a binding geometry with the new MBR
    % and an undefined value
    First#geometry{mbr=BindingMBR, value=undefined}.


% Returns the area of the given geometry
-spec area(#geometry{}) -> float().
area(Geometry) ->
    lists:foldl(fun({MinV, MaxV}, Sum) ->
        Sum * (MaxV - MinV)
    end, 1, Geometry#geometry.mbr).


% Returns the number of edges in a given geometry
-spec num_edges(#geometry{}) -> integer().
num_edges(Geometry) ->
    N = Geometry#geometry.dimensions,
    trunc(math:pow(2, N - 1) * N).


% Returns the margin of the given geometry
-spec margin(#geometry{}) -> float().
margin(Geometry) ->
    % Sum length of each axis
    AxisSum = lists:foldl(fun({MinV, MaxV}, Sum) ->
        Sum + (MaxV - MinV)
    end, 0, Geometry#geometry.mbr),

    % Figure out the scaling factor,
    Scale = num_edges(Geometry) / Geometry#geometry.dimensions,

    % Multiple the sum by the scale
    AxisSum * Scale.


% Returns the overlapping geometry or 0
-spec intersect(#geometry{}, #geometry{}) -> #geometry{} | undefined.
intersect(Geo1, Geo2) ->
    Overlap = lists:zipwith(fun ({MinA, MaxA}, {MinB, MaxB}) ->
            Min = max(MinA, MinB),
            Max = min(MaxA, MaxB),
            case Min =< Max of
                true -> {Min, Max};
                _ -> undefined
            end
       end, Geo1#geometry.mbr, Geo2#geometry.mbr),

    % Check if any of the axes did not overlap
    case lists:member(undefined, Overlap) of
        true -> undefined;
        _ -> Geo1#geometry{mbr=Overlap, value=undefined}
    end.


% Returns the center of a given geometry
-spec center(#geometry{}) -> #geometry{}.
center(Geo) ->
    % Average to get the center points along each axis
    CenterPoint = lists:map(fun({Min, Max}) ->
        Avg = (Min + Max) / 2.0,
        {Avg, Avg}
    end, Geo#geometry.mbr),
    Geo#geometry{mbr=CenterPoint, value=undefined}.


% Returns the distance between two points, or uses
% the minimum bounds for non-point geometries.
-spec distance(#geometry{}, #geometry{}) -> float().
distance(Geo1, Geo2) ->
    % Get the square of distances along each axis
    Distances = lists:zipwith(fun({MinA, _}, {MinB, _}) ->
        math:pow(MinB - MinA, 2)
    end, Geo1#geometry.mbr, Geo2#geometry.mbr),

    % Sum the distances
    SumDistance = lists:sum(Distances),

    % The square root is the Euclidean distance
    math:sqrt(SumDistance).


% Verifies that the max axis value is greater or equal to the minimum
valid_axes([], Length) -> {ok, Length};
valid_axes([{MinV, MaxV}| Other], Length) ->
    if
        MaxV >= MinV -> valid_axes(Other, Length + 1);
        true -> {error, {badarg, {MinV, MaxV}}}
    end.


