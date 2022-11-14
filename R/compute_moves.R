#' Calculate the moves and sub-states.
#'
#' Computes the moves made from a solution in the item table and stores them so
#' they full solution can be saved in the solution table. A helper function for
#' clean_item_table.
#'
#' @param temp_solution
#' @param n
#'
#' @return
#' @export

compute_moves <- function(temp_solution, n)
{
  move_cols <- c("item", "capacities", "start", "goal", "solution",
                 "length", "first_moves", "state",
                 "move_num", "move", "distance")
  move_table <- data.frame(matrix(nrow=1, ncol=length(move_cols)))
  colnames(move_table) <- move_cols
  moves_to_make <- temp_solution$length
  fill_columns <- c("capacities", "start", "goal",
                    "solution", "length", "first_moves")
  move_table$capacities <- list(c(temp_solution$j1cap,
                                  temp_solution$j2cap,
                                  temp_solution$j3cap))
  move_table$start <- list(c(temp_solution$j1start,
                             temp_solution$j2start,
                             temp_solution$j3start))
  move_table$goal <- list(c(temp_solution$j1goal,
                            temp_solution$j2goal,
                            temp_solution$j3goal))


  move_table$solution <- n
  move_table$length <- temp_solution$length
  move_table$first_moves <- temp_solution$start_moves
  move_table$state <- move_table$start
  move_table$move_num <- 0

  move_table$move <- list(c(temp_solution$m1j1, temp_solution$m1j2))

  first_move_index <- match("m1j1", names(temp_solution))

  move_table$distance <- move_table$length - move_table$move_num



  if (moves_to_make > 0) {
    for (i in 1:moves_to_make) {

      move_table <- dplyr::add_row(move_table)
      move_table$move_num[i+1] <- i
      move_table$state[i+1] <- list(make_move(c(move_table$move[[i]][1],
                                                move_table$move[[i]][2]),
                                              c(move_table$state[[i]][1],
                                                move_table$state[[i]][2],
                                                move_table$state[[i]][3]),
                                              c(move_table$capacities[[i]][1],
                                                move_table$capacities[[i]][2],
                                                move_table$capacities[[i]][3])))

      move_table <- tidyr::fill(move_table, fill_columns)

      move_table$distance[i+1] <-
        move_table$length[i+1] - move_table$move_num[i+1]


      move_table$move[i+1] <- list(c(
        temp_solution[[first_move_index+(i*2)]],
        temp_solution[[first_move_index+(i*2)+1]]))

    }
  }


  move_table <- mutate(move_table, move=lag(move))

  return(move_table)

}
