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
new(Dimensions, _) when Dimensions =< 1 -> {error, badarg};
new(Dimensions, Params) -> #rtree{dimensions=Dimensions, params=Params}.


% Inserts a new geometric point into the R* tree and
% returns a new tree
-spec insert(rtree(), geometry()) -> rtree() | {error, dimensionality}.
insert(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

insert(_Tree, _Geometry) -> ok.


% Removes a geometric point from the R* tree and
% returns a new tree
-spec delete(rtree(), geometry()) -> rtree().
delete(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

delete(_Tree, _Geometry) -> ok.


% Searches for the geometries contained or intersecting
% the given geometry
-spec search_within(rtree(), geometry()) -> [geometry()].
search_within(#rtree{dimensions=TD}, #geometry{dimensions=GD}) when TD =/= GD ->
    {error, dimensionality};

search_within(_Tree, _Geometry) -> ok.


% Searches for the K-nearest neighbors to the given
% geometry.
-spec search_nearest(rtree(), geometry(), integer()) -> [geometry()].
search_nearest(#rtree{dimensions=TD}, #geometry{dimensions=GD}, _) when TD =/= GD ->
    {error, dimensionality};
search_nearest(_, _, Nearest) when Nearest =< 0 ->
    {error, badarg};

search_nearest(_Tree, _Geometry, _K) -> ok.


% Searches for the geometries contained or intersecting
% the given geometry
-spec search_around(rtree(), geometry(), float()) -> [geometry()].
search_around(#rtree{dimensions=TD}, #geometry{dimensions=GD}, _) when TD =/= GD ->
    {error, dimensionality};

search_around(_Tree, _Geometry, _Distance) -> ok.


