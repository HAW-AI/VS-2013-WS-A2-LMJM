%% Records and Macros used by nodes

-record(state, {
  name,
  edges,
  status
}).

-record(node, {
  name,
  pid
}).

-record(edge, {
  node_1,
  node_2,
  weight,
  type
}).
