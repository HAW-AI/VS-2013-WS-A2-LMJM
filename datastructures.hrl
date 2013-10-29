%% Records and Macros used by nodes

-record(state, {
  name,
  status,
  edges,
  best_edge,
  best_weight,
  test_edge,
  in_branch,
  find_count,
  fragment_name,
  fragment_level
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
