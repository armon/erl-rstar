Erlang Rstar [![Build Status](https://travis-ci.org/armon/erl-rstar.png)](https://travis-ci.org/armon/erl-rstar)
=========

This application provides a pure Erlang implementation of the R*-tree
data structure. The R*-tree structure is particularly suited for indexing
multi-dimensional data, with relatively low dimensionality. It is a commonly
used data structure for geo-spacial data for this reason. In addition to
efficiently supporting window queries, it can also quickly find the nearest
neighbors of a point.

This implementation is in-memory only and does not provide builtin support
for disk-backed usaged.


Modules
-----

* rstar : The primary top level module used to manipulate the tree
* rstar_geometry : Used to modify the geometry type associated with trees
* rstar_util: Contains a various utility methods that may be useful


Usage
-----

Below are some examples of usage.
Simple creation of trees and population of points:

    % Creates a two dimensional R-tree
    Tree = rstar:new(2),

    % Create a point, at X:1, Y:3, opaque value foo
    Point = rstar_geometry:point2d(1, 3, foo),

    % Insert into the tree, returning a new tree
    T2 = rstar:insert(Tree, Point),

    % Delete the point
    T3 = rstar:delete(T2, Point).


Creating various geometries:

    % Create a 2D box from the origin to 2,2
    Box = rstar_geometry:new(2, [{0, 2}, {0, 2}], small_box),

    % Create a 3D box
    Box3D = rstar_geometry:new(3, [{2, 3}, {2, 3}, {2, 3}], three_d_box),

    % Create a 3D point
    Point3D = rstar_geometry:point3d(0, 1, 2, point3d),

    % Create a 4D point at the origin
    Origin4D = rstar_geometry:origin(4).


Various queries:

    % Create a tree and populate it
    T = rstar:new(2),
    ....
    TFinal = rstar:insert(...),

    % Do box query
    Box = rstar_geometry:new(2, [{0, 2}, {0, 2}], small_box),
    Matching = rstar:search_within(TFinal, Box),

    % Do a circular query around a point within a distance of 10 units
    % Distance is Euclidean distance
    Point = rstar_geometry:point2d(1, 3, undefined),
    Matching = rstar:search_around(TFinal, Point, 10.0),

    % Find the nearest 20 points
    Matching = rstar:search_nearest(TFinal, Point, 20),


References
----------

Related works:

* [R-trees: A dynamic index structure for spacial searching](http://www.cs.jhu.edu/~misha/ReadingSeminar/Papers/Guttman84.pdf)
* [The R*-tree: An Efficient and Robust Access Method for Points and Rectangles](http://www.cs.ucr.edu/~tsotras/cs236/F11/rstar.pdf)
* [Nearest Neighbor Queries](http://postgis.refractions.net/support/nearestneighbor.pdf)
* [Enhanced Nearest Neighbor Search on the R-tree](http://www.cse.cuhk.edu.hk/~adafu/Pub/rtree.ps)

