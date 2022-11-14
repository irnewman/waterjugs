
#' Select the move columns from raw data file.
#'
#' @param trial A row of the raw data file corresponding to a single trial.
#'
#' @return The water movement columns from the trial.
#' @export

select_moves <- function(trial)
{
  first_col <- "Move1Jug1"
  last_col <-   colnames(trial)[ncol(trial)]
  first_index <- match(first_col, names(trial))
  last_index <- match(last_col, names(trial))
  if (is.na(first_index)) {
    moves <- data.frame(matrix(nrow=1, ncol=5))
    colnames(moves) <- c("Move1Jug1",
                         "Move1Jug2",
                         "Move1Time1",
                         "Move1Time2",
                         "Move1Amount")
    moves[1,] <- NA
  } else {
    moves <- trial[first_index:last_index]
  }
  return(moves)
}
