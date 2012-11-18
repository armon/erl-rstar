
% A Geometry record contains a dimensionality
% a MBR (minimum bounding rectagle), which is a list
% of min/max values per dimension, and an opaque value.
-record(geometry, {
    dimensions,
    mbr,
    value
}).


