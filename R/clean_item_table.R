#' Reduce solutions to optimal solutions.
#'
#' From an item table of computed solutions for a given problem, this function
#' reduces those solutions to only the shortest solution(s).
#'
#' @param item_table A table of all successful solutions for the current item.
#'
#' @return
#' @export

clean_item_table <- function(item_table)
{
  fill_columns <- c("start_moves",
                    "j1cap", "j2cap", "j3cap",
                    "j1start", "j2start", "j3start",
                    "j1goal", "j2goal", "j3goal")

  # check if unsolvable
  if (nrow(item_table) == 1 && is.na(item_table$length[1])) {
    optimal_length <- 0
    temp_solutions <- item_table
    temp_solutions$length[1] <- 0
    optimal_solutions <- compute_moves(temp_solutions, optimal_length)
  } else {
    optimal_length <- min(item_table$length, na.rm=TRUE)
    # reduce table to only optimal length solutions
    temp_solutions <- tidyr::fill(item_table, fill_columns) %>%
      filter(length==optimal_length)

    # compute moves for each solution
    for (n in 1:nrow(temp_solutions)) {
      moves_filled <- compute_moves(temp_solutions[n,], n)
      moves_filled$item <- n
      if (n == 1) {
        optimal_solutions <- moves_filled
      }
      if (n > 1) {
        optimal_solutions <- rbind(optimal_solutions, moves_filled)
      }
    }
  }

  # empty the item table for the next item to be solved
  item_table <<- item_table[1,]
  item_table[1,] <<- NA
  return(optimal_solutions)
}



