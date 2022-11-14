#' Title
#'
#' @param start_state
#' @param moves
#' @param jug_caps
#'
#' @return
#' @export

clean_move_states <- function(start_state, moves, jug_caps)
{
  number_of_moves <- (ncol(moves) / 5)
  moves_made <- moves %>%
    select(grep("Jug", names(moves)))
  move_cols <- colnames(moves_made)[c(T,F)]
  # jug states after each move
  move_states <- data.frame(matrix(nrow=1, ncol=number_of_moves))
  colnames(move_states) <- move_cols
  colnames(move_states) <- gsub(pattern = "Jug1",
                                replacement = ".state",
                                x = names(move_states))
  # calculate jug states
  current_state <- start_state
  for (i in 1:ncol(move_states)) {
    j <- i*2
    move <- c(moves_made[[j-1]], moves_made[[j]])
    if (all(!is.na(move))) {
      move_states[[i]] <- list(make_move(move, current_state, jug_caps))
      current_state <- as.numeric(unlist(move_states[[i]]))
    }
  }
  return(move_states)
}
