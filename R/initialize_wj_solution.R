#' Save values to water jug solution table.
#'
#' this sets features of the item table for later computation, so a helper
#'
#'
#'
#' @param current_state A 3-integer vector, starting level of water in each jug.
#' @param goal_state A 3-integer vector, the target level of water in each jug.
#' @param jug_caps A 3-integer vector, the maximum capacity in each jug.
#'
#' @return Nothing.
#' @export


initialize_wj_solution <- function(current_state, goal_state, jug_caps) {

  # starting state
  set(item_table, i = nrow(item_table), j = 'j1start', value = current_state[1])
  set(item_table, i = nrow(item_table), j = 'j2start', value = current_state[2])
  set(item_table, i = nrow(item_table), j = 'j3start', value = current_state[3])

  # goal state
  set(item_table, i = nrow(item_table), j = 'j1goal', value = goal_state[1])
  set(item_table, i = nrow(item_table), j = 'j2goal', value = goal_state[2])
  set(item_table, i = nrow(item_table), j = 'j3goal', value = goal_state[3])

  # jug capacities
  set(item_table, i = nrow(item_table), j = 'j1cap', value = jug_caps[1])
  set(item_table, i = nrow(item_table), j = 'j2cap', value = jug_caps[2])
  set(item_table, i = nrow(item_table), j = 'j3cap', value = jug_caps[3])

  # number of initial moves available
  set(item_table, i = nrow(item_table), j = 'start_moves',
      value = length(available_moves(current_state, jug_caps)))

}
