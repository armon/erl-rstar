
% A Geometry record contains a dimensionality
% a MBR (minimum bounding rectagle), which is a list
% of min/max values per dimension, and an opaque value.
-record(geometry, {
    dimensions,
    mbr,
    value
}).

% Contains the parameters of the R*-tree. Contains a default configuration.
-record(rt_params, {
    max=8,
    min=2,
    reinsert=2
}).

% A rtree object contains the root node and the dimensionality
% of the entire tree. All the geometries associated with the tree
% must have the same dimensionality
-record(rtree, {
    dimensions,
    params=#rt_params{},
    root
}).

% A leaf record contains inserted Geometries and associated values
-record(leaf, {
    entries=[]
}).

% A node record contains pointers to other nodes or leaves
-record(node, {
    children=[]
}).

