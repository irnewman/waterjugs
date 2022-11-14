#' Calculate start states.
#'
#' Computes a list of all the optional starting states for the water jug
#' capacities and total units of water specified.#'
#'
#' @param jug_caps A 3-integer vector, the maximum capacity in each jug.
#' @param total_water Total units of water in the problem.
#' @return Returns list of the starting states.
#' @export

start_state_calc <- function(jug_caps, total_water)
{
  start_list <- c()
  j1min <- ifelse(total_water <= (jug_caps[2] + jug_caps[3]),
                  0,
                  jug_caps[1] - (jug_caps[2] + jug_caps[3]))

  # compute possible j2 and j3 values for each j1 value
  for (i in j1min:jug_caps[1]) {
    subtotal <- total_water - i  # amount of water forced into j2 and j3
    j2min <- subtotal - jug_caps[3]
    for (j in j2min:jug_caps[2]) {
      j3start <- total_water - i - j
      temp_start_list <- c(i, j, j3start)
      if (temp_start_list[1] > -1 &&
          temp_start_list[2] > -1 &&
          temp_start_list[3] > -1) {
        start_list <- rbind(start_list, temp_start_list)
      }
    }
  }

  # return only unique values
  colnames(start_list) <- c("j1", "j2", "j3")
  s_list <- unique(start_list[,c('j1', 'j2', 'j3')])
  return(s_list)
}
