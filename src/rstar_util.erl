-module(rstar_util).
-export([random_tree/3, random_tree/4, random_geo/2]).
-include("../include/rstar.hrl").


% Generates a random tree with a given dimensionality,
% bound on points, and number of elements
-spec random_tree(integer(), integer(), integer()) -> #rtree{}.
random_tree(Dimensions, Bounds, Num) ->
    random_tree(rstar:new(Dimensions), Dimensions, Bounds, Num).

% Recursively builds the tree
random_tree(Tree, _, _, 0) -> Tree;
random_tree(Tree, Dimensions, Bounds, Num) ->
    T = rstar:insert(Tree, random_geo(Dimensions, Bounds)),
    random_tree(T, Dimensions, Bounds, Num - 1).


% Generates a random geometry with given dimentionality
% and axis bounds
-spec random_geo(integer(), integer()) -> #geometry{}.
random_geo(Dimensions, Bounds) ->
    Rand = random:uniform(Bounds),
    MBR = [{Rand, Rand} || _D <- lists:seq(1, Dimensions)],
    #geometry{dimensions=Dimensions, mbr=MBR, value=Rand}.

