#' Calculate goal values
#'
#' Determines the list of optional target states for the water jug specifications.
#' @param jug_caps A 3-integer vector, the maximum capacity in each jug.
#' @param total_water Total units of water in the problem.
#' @return Returns list of the goal states.
#' @export

# generates list of usable goal states based on jug capacities and total starting water (two vectors as arguments)
goal_state_calc <- function(jug_caps, total_water)
{
  goal_list <- c()
  j1min <- ifelse(total_water <= (jug_caps[2] + jug_caps[3]), 0, jug_caps[1] - (jug_caps[2] + jug_caps[3]))

  for (i in j1min:jug_caps[1]) {
    subtotal <- total_water - i  # amount of water forced into j2 and j3
    j2min <- subtotal - jug_caps[3]

    for (j in j2min:jug_caps[2]) {

      j3goal <- total_water - i - j # BUG HERE, ALLOWS FOR LARGER THAN j3max TO BE INCLUDED (see next line)
      # ifelse(total_water - i - j <= jug_caps[3], total_water - i - j, jug_caps[3])
      temp_goal_list <- c(i, j, j3goal)

      if (temp_goal_list[1] > -1 && temp_goal_list[2] > -1 && temp_goal_list[3] > -1) {
        goal_list <- rbind(goal_list, temp_goal_list)
      }
    }
  }

  colnames(goal_list) <- c("j1", "j2", "j3")
  return(goal_list)
}
