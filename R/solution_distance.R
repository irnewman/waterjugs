
#' Compute distance from current state to goal.
#'
#' This is for computing a DV from the raw data.
#'
#' @param max_moves Maximum possible moves to test.
#' @param trial_data A row of the trial data frame.
#' @param solutions A data frame of the optimal solution(s) of that item.
#'
#' @return The smallest number of moves to solution.
#' @export

solution_distance <- function(max_moves, trial_data, solutions)
{
  start_values <- c(trial_data$Jug1Start,
                    trial_data$Jug2Start,
                    trial_data$Jug3Start)
  current_values <- start_values

  distance_to_solution <- -1  # init at impossible value

  if (max_moves == 0) {
    for (m in 1:nrow(solutions)) {  # search rows of solutions
      if (isTRUE(all.equal(current_values, c(
        current_solutions$j1_state[m],
        current_solutions$j2_state[m],
        current_solutions$j3_state[m])))) {

        distance_to_solution <- current_solutions$distance[m]
      }
    }
    return(distance_to_solution)
  }



  for (j in 1:max_moves){





    #print(distance_to_solution)

    # jug moved from, to, and the amount
    jug1 <- paste0("Move", j, "Jug1")
    jug2 <- paste0("Move", j, "Jug2")
    move_amount <- paste0("Move", j, "Amount")

    # if there are more moves
    if (jug1 %in% colnames(trial_data) &&
        !is.na(trial_data[k, jug1]) &&
        !is.na(trial_data[k, jug2])) {

      # jug to move from and move to
      j1 <- trial_data[jug1][[1]]
      j2 <- trial_data[jug2][[1]]

      # amount to be moved
      amount <- trial_data[move_amount]

      # update values
      current_values[j1] <- current_values[j1] - amount
      current_values[j2] <- current_values[j2] + amount



    }

    for (m in 1:nrow(solutions)) {  # search rows of solutions
      if (isTRUE(all.equal(current_values, c(
        current_solutions$j1_state[m],
        current_solutions$j2_state[m],
        current_solutions$j3_state[m])))) {

        distance_to_solution <- current_solutions$distance[m]
      }
    }

  }

  return(distance_to_solution)

}
