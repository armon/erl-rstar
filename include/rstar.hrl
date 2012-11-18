
% A Geometry record contains a dimensionality
% a MBR (minimum bounding rectagle), which is a list
% of min/max values per dimension, and an opaque value.
-record(geometry, {
    dimensions,
    mbr,
    value
}).

% A rtree object contains the root node and the dimensionality
% of the entire tree. All the geometries associated with the tree
% must have the same dimensionality
-record(rtree, {
    dimensions,
    root
}).

