-module(rstar).
-export([new/1, new/2, insert/2, delete/2, search_within/2, search_nearest/3, search_around/3]).
-export_type([rtree/0, geometry/0]).

-include("../include/rstar.hrl").

% Expose type references
-type rtree() :: #rtree{}.
-type geometry() :: #geometry{}.


% Returns a new empty R* tree of the specified dimensionality and default parameters
-spec new(integer()) -> rtree() | {error, badarg}.
new(Dimensions) -> new(Dimensions, #rt_params{}).


% Returns a new empty R* tree of the specified dimensionality and parameters
-spec new(integer(), #rt_params{}) -> rtree() | {error, badarg}.
new(Dimensions, _) when Dimensions < 1 -> {error, badarg};
new(Dimensions, Params) ->
    RootGeo = rstar_geometry:origin(Dimensions),
    Root = RootGeo#geometry{value=#leaf{}},
    #rtree{dimensions=Dimensions, params=Params, root=Root}.


% Inserts a new geometric point into the R* tree and
% returns a new tree
-spec insert(rtree(), geometry()) -> rtree() | {error, dimensionality}.
insert(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

insert(Tree, Geometry) ->
    NewRoot = rstar_insert:insert(Tree#rtree.params, Tree#rtree.root, Geometry),
    Tree#rtree{root=NewRoot}.


% Removes a geometric point from the R* tree and
% returns a new tree
-spec delete(rtree(), geometry()) -> not_found | rtree().
delete(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

delete(Tree, Geometry) ->
    case rstar_delete:delete(Tree#rtree.params, Tree#rtree.root, Geometry) of
        not_found -> not_found;
        NewRoot -> Tree#rtree{root=NewRoot}
    end.

% Searches for the geometries contained or intersecting
% the given geometry
-spec search_within(rtree(), geometry()) -> [geometry()].
search_within(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

search_within(Tree, Geometry) ->
    rstar_search:search_within(Tree#rtree.root, Geometry).


% Searches for the K-nearest neighbors to the given
% geometry.
-spec search_nearest(rtree(), geometry(), integer()) -> [geometry()].
search_nearest(#rtree{dimensions=TD}, #geometry{dimensions=GD}, _) when TD =/= GD ->
    {error, dimensionality};
search_nearest(_, _, Nearest) when Nearest =< 0 ->
    {error, badarg};

search_nearest(Tree, Geometry, K) ->
    rstar_search:search_near(Tree#rtree.root, Geometry, K).


% Searches for the geometries contained or intersecting
% the given geometry
-spec search_around(rtree(), geometry(), float()) -> [geometry()].
search_around(#rtree{dimensions=TD}, #geometry{dimensions=GD}, _) when TD =/= GD ->
    {error, dimensionality};

search_around(Tree, Geometry, Distance) ->
    % Build and expanded geometry around the point
    NewMBR = [{Min - Distance, Max + Distance} || {Min, Max} <- Geometry#geometry.mbr],
    NewGeo = Geometry#geometry{mbr=NewMBR},

    % Perform a search_within
    PrimaryResults = rstar_search:search_within(Tree#rtree.root, NewGeo),

    % Apply a secondary filter
    lists:filter(fun (R) ->
        rstar_geometry:distance(R, Geometry) =< Distance
    end, PrimaryResults).


