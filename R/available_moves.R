#' Determine available moves from current state.
#'
#' Calculates the list of possible moves that are available from the current
#' state of water in each jug.
#'
#' @param current_state A 3-integer vector, the current water in each jug.
#' @param jug_caps A 3-integer vector, the maximum capacity in each jug.
#' @return A list of move options.
#' @export

available_moves <- function(current_state, jug_caps)
{
  move_options <- list()  # initialize list
  move_from <- which(current_state > 0)  # which jugs have water
  move_to <- which(current_state < jug_caps) # which jugs aren't full
  move_matrix <- outer(move_from, move_to, "!=")  # matrix of valid moves
  rownames(move_matrix) <- move_from
  colnames(move_matrix) <- move_to
  which_moves <- which(move_matrix == TRUE, arr.ind = TRUE)

  # compute the available moves as two-item vectors in a list
  if (length(move_from) > 0 && length(move_to) > 0) {
    temp_moves <- cbind(as.numeric(rownames(move_matrix)[which_moves[, "row"]]),
                        as.numeric(colnames(move_matrix)[which_moves[, "col"]]))
    move_options <- lapply(1:nrow(temp_moves), function(x) temp_moves[x, 1:2])
  }
  return(move_options)
}
