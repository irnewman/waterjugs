#' Make the selected move.
#'
#' Computes the move of water from the source jug to the target jug. The amount
#' of water poured is that maximum units that will fit in the target jug.
#'
#' @param move A 2-integer vector, index 1 is source, index 2 is target.
#' @param current_state A 3-integer vector, current level of water in each jug.
#' @param jug_caps A 3-integer vector, the maximum capacity of each jug.
#' @return A vector of water units in each jug after the move.
#' @export

make_move <- function(move, current_state, jug_caps)
{
  source_jug <- move[1]
  target_jug <- move[2]
  source_current <- current_state[source_jug]
  target_current <- current_state[target_jug]

  # maximum move amount
  move_amount_max <- jug_caps[target_jug] - target_current

  # compute amount to move from source to target
  if (source_current < move_amount_max){
    move_amount <- source_current
  } else {
    move_amount <- move_amount_max
  }

  # save move to updated vector
  updated_state <- current_state
  updated_state[source_jug] <- updated_state[source_jug] - move_amount
  updated_state[target_jug] <- updated_state[target_jug] + move_amount

  return(updated_state)
}
