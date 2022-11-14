#' Has the goal been reached
#'
#' Determines if the goal state has been reached yet and
#' returns true if current and goal jug values are equal.
#'
#' @param current_state A 3-integer vector, the current level of water in each jug.
#' @param goal_state A 3-integer vector, the target level of water in each jug.
#' @return True if goal reached, otherwise false.
#' @export

goal_reached <- function(current_state, goal_state)
{
  ifelse((all(length(current_state) == length(goal_state)) &&
            all(current_state == goal_state)),
         return(TRUE),
         return(FALSE)
  )
}
