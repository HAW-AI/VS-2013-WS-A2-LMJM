%% Records and Macros used by nodes

-record(state, {
  name,
  status = sleeping,
  edges,
  best_edge,
  best_weight,
  test_edge,
  in_branch,
  find_count = 0,
  fragment_name,
  fragment_level = 0
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
