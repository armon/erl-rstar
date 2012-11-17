-module(rstar).
-export([new/0, insert/2, delete/2, searchWithin/2, searchNearest/3, searchAround/3]).
-export_type([rtree/0, geometry/0]).

% Expose opaque type references
-type rtree() :: any().
-type geometry() :: any().

% Returns a new empty R* tree
-spec new() -> rtree().
new() -> ok.

% Inserts a new geometric point into the R* tree and
% returns a new tree
-spec insert(rtree(), geometry()) -> rtree().
insert(_Tree, _Geometry) -> ok.

% Removes a geometric point from the R* tree and
% returns a new tree
-spec delete(rtree(), geometry()) -> rtree().
delete(_Tree, _Geometry) -> ok.

% Searches for the geometries contained or intersecting
% the given geometry
-spec searchWithin(rtree(), geometry()) -> list(geometry()).
searchWithin(_Tree, _Geometry) -> ok.

% Searches for the K-nearest neighbors to the given
% geometry.
-spec searchNearest(rtree(), geometry(), integer()) -> list(geometry()).
searchNearest(_Tree, _Geometry, _K) -> ok.

% Searches for the geometries contained or intersecting
% the given geometry
-spec searchAround(rtree(), geometry(), float()) -> list(geometry()).
searchAround(_Tree, _Geometry, _Distance) -> ok.

