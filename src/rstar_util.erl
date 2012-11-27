-module(rstar_util).
-export([random_tree/3, random_geo/2]).
-include("../include/rstar.hrl").


% Generates a random tree with a given dimensionality,
% bound on points, and number of elements
-spec random_tree(integer(), integer(), integer()) -> #rtree{}.
random_tree(Dimensions, Bounds, Num) ->
    % Get random points
    Points = [random_geo(Dimensions, Bounds) || _N <- lists:seq(1, Num)],

    % Insert all the points, return new tree
    lists:foldl(fun(G, T) ->
        rstar:insert(T, G)
    end, rstar:new(Dimensions), Points).


% Generates a random geometry with given dimentionality
% and axis bounds
-spec random_geo(integer(), integer()) -> #geometry{}.
random_geo(Dimensions, Bounds) ->
    Rand = random:uniform(Bounds),
    MBR = [{Rand, Rand} || _D <- lists:seq(1, Dimensions)],
    #geometry{dimensions=Dimensions, mbr=MBR, value=Rand}.

