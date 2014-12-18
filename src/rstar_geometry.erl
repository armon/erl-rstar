-module(rstar_geometry).
-export([new/3, origin/1, point2d/3, point3d/4, bounding_box/1,
         area/1, intersect/2, num_edges/1, margin/1, center/1,
         distance/2, min_dist/2, value/1]).

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
bounding_box([First | More]) ->
    bounding_box_r(First, More, First#geometry.mbr);
bounding_box([]) -> {error, badarg}.


% Recursively handles each geometry
bounding_box_r(G, [First | MoreGeo], Bound) ->
    NB = bounding_mbr(First#geometry.mbr, Bound, []),
    bounding_box_r(G, MoreGeo, NB);
bounding_box_r(G, [], Bound) ->
    G#geometry{mbr=Bound, value=undefined}.


% Recursively handles each axis
bounding_mbr([{MinA, MaxA}|More1], [{MinB, MaxB}|More2], Bound) ->
    Min = if
        MinA < MinB -> MinA;
        true -> MinB
    end,
    Max = if
        MaxA < MaxB -> MaxB;
        true -> MaxA
    end,
    NB = [{Min, Max} | Bound],
    bounding_mbr(More1, More2, NB);
bounding_mbr([], [], Bound) -> lists:reverse(Bound).


% Returns the area of the given geometry
-spec area(#geometry{}) -> float().
area(Geometry) ->
    area_r(Geometry#geometry.mbr, 1).

area_r([{MinV, MaxV} | More], Sum) ->
    area_r(More, Sum * (MaxV - MinV));
area_r([], Sum) -> Sum.

% Returns the number of edges in a given geometry
-spec num_edges(#geometry{}) -> integer().
num_edges(Geometry) ->
    N = Geometry#geometry.dimensions,
    trunc(math:pow(2, N - 1) * N).


% Returns the margin of the given geometry
-spec margin(#geometry{}) -> float().
margin(Geometry) ->
    % Sum length of each axis
    AxisSum = margin_r(Geometry#geometry.mbr, 0),

    % Figure out the scaling factor,
    Scale = num_edges(Geometry) / Geometry#geometry.dimensions,

    % Multiple the sum by the scale
    AxisSum * Scale.

margin_r([{MinV, MaxV} | More], Sum) ->
    margin_r(More, Sum + (MaxV - MinV));
margin_r([], Sum) -> Sum.


% Returns the overlapping geometry or 0
-spec intersect(#geometry{}, #geometry{}) -> #geometry{} | undefined.
intersect(Geo1, Geo2) ->
    intersect_r(Geo1, Geo1#geometry.mbr, Geo2#geometry.mbr, []).

intersect_r(G, [{MinA, MaxA} | More1], [{MinB, MaxB} |More2], Intersect) ->
    Min = if
        MinA < MinB -> MinB;
        true -> MinA
    end,
    Max = if
        MaxA < MaxB -> MaxA;
        true -> MaxB
    end,
    if
        Min =< Max ->
            intersect_r(G, More1, More2, [{Min, Max} | Intersect]);
        true -> undefined
    end;

intersect_r(G, [], [], Intersect) ->
    G#geometry{mbr=lists:reverse(Intersect), value=undefined}.


% Returns the center of a given geometry
-spec center(#geometry{}) -> #geometry{}.
center(Geo) ->
    % Average to get the center points along each axis
    CenterPoint = [{(Min+Max)/2.0, (Min+Max)/2.0} ||
            {Min, Max} <- Geo#geometry.mbr],
    Geo#geometry{mbr=CenterPoint, value=undefined}.


% Returns the distance between two points, or uses
% the minimum bounds for non-point geometries.
-spec distance(#geometry{}, #geometry{}) -> float().
distance(Geo1, Geo2) ->
    % Sum the square distances
    SumDistance = distance_r(Geo1#geometry.mbr,
                             Geo2#geometry.mbr, 0),

    % The square root is the Euclidean distance
    math:sqrt(SumDistance).

distance_r([{MinA, _} | More1], [{MinB, _} | More2], Sum) ->
    distance_r(More1, More2, math:pow(MinB - MinA, 2) + Sum);
distance_r([], [], Sum) -> Sum.


% Returns the minimum distance between a point and rectangle
-spec min_dist(#geometry{}, #geometry{}) -> float().
min_dist(Point, Rect) ->
    min_dist_r(Point#geometry.mbr, Rect#geometry.mbr, 0.0).

min_dist_r([{P, _} | More1], [{MinR, MaxR} | More2], Sum) ->
    Val = if
        P < MinR -> math:pow(MinR - P, 2);
        P > MaxR -> math:pow(P - MaxR, 2);
        true -> 0.0
    end,
    min_dist_r(More1, More2, Sum + Val);
min_dist_r([], [], Sum) -> Sum.


% Verifies that the max axis value is greater or equal to the minimum
valid_axes([], Length) -> {ok, Length};
valid_axes([{MinV, MaxV}| Other], Length) ->
    if
        MaxV >= MinV -> valid_axes(Other, Length + 1);
        true -> {error, {badarg, {MinV, MaxV}}}
    end.

% Returns the opaque value of the given geometry
-spec value(#geometry{}) -> any().
value(Geometry) ->
    Geometry#geometry.value.

