%% Records and Macros used by nodes

-record(state, {
  name,
  branch_edge,
  rejected_edges,
  basic_edges
}).

-record(node, {
  name,
  pid
}).

-record(edge, {
  node_1,
  node_2,
  weight
}).
