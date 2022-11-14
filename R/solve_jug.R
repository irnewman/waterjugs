#' Finds the optimal solution
#'
#' A recursive function that finds the optimal solution for a water jug problem.
#' @param current_state A 3-integer vector, the current level of water in each jug.
#' @param goal_state A 3-integer vector, the target level of water in each jug.
#' @param jug_caps A 3-integer vector, the maximum capacity in each jug.
#' @param move_counter Set to 1 when calling this function.
#' @param branch_table TO DO
#' @param solution_table TO DO
#' @return Returns nothing.
#' @export


# NOTE: need to add max_tries to this, I think
# NOTE: need to make this record all the equally optimal solutions
# SHOULD PROBABLY CHANGE HOW MOVECOUNTER WORKS TOO
# HAVE IT RETURN THE DURATION TOO?
# AUTOMATE HOW THE BRANCH AND SOLUTION TABLES ARE INITIALLY CALCULATED?

# #' @param max_tries Maximum length of each branch of the solution tree.


# can i write code here as global variables? SEE BELOW

# pkg.globals <- new.env()
#
# pkg.globals$data_path <- "data"
#
# set_data_path <- function(path) {
#   pkg.globals$data_path <- path
# }


# solves the item
solve_jug <- function(current_state, goal_state, jug_caps, move_counter, branch_table)
{

  # limit attempts
  if (move_counter > max_tries) {
    return()
  }

  # record initial values
  if (move_counter == 1) {
    initialize_wj_solution(current_state, goal_state, jug_caps)
  }

  # if goal reached
  if (goal_reached(current_state, goal_state)) {


    finalize_wj_solution(branch_table, move_counter)

    # if (!is.na(solution_table$length) && solution_length < solution_table$length) {
    #   # clear values
    #   for (w in 12:length(solution_table)) {
    #     set(solution_table, i = nrow(solution_table), j = as.integer(w), value = NA)
    #   }
    # }

    # if done, save several values
    # if (is.na(solution_table$length) || solution_length < solution_table$length) {
    #
    #   # solution length
    #   #set(solution_table, i = nrow(solution_table), j = 2L, value = solution_length)
    #   # record solution moves
    #   for (s in 1:(solution_length*2)) {
    #     set(solution_table, i = nrow(solution_table), j = as.integer(s+12), value = branch_table[[s]])
    #   }
    # }
    return() # for goal reached
  }

  # compute next moves
  next_moves <- available_moves(current_state, jug_caps)

  # current move columns
  j1 <- paste("m", move_counter, "j1", sep="")
  j2 <- paste("m", move_counter, "j2", sep="")


  for (r in 1:length(next_moves)) {
    # make the move before calling function recursively
    updated_state <- make_move(next_moves[[r]], current_state, jug_caps)
    # record the current move in the branch table
    set(branch_table, 1L, j1, next_moves[[r]][1])
    set(branch_table, 1L, j2, next_moves[[r]][2])
    # recursive call
    solve_jug(updated_state, goal_state, jug_caps, move_counter+1, branch_table)
  }
}
