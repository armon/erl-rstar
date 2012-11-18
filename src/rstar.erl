-module(rstar).
-export([new/0, insert/2, delete/2, search_within/2, search_nearest/3, search_around/3]).
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
-spec search_within(rtree(), geometry()) -> [geometry()].
search_within(_Tree, _Geometry) -> ok.

% Searches for the K-nearest neighbors to the given
% geometry.
-spec search_nearest(rtree(), geometry(), integer()) -> [geometry()].
search_nearest(_Tree, _Geometry, _K) -> ok.

% Searches for the geometries contained or intersecting
% the given geometry
-spec search_around(rtree(), geometry(), float()) -> [geometry()].
search_around(_Tree, _Geometry, _Distance) -> ok.

