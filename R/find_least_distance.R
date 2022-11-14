
#' Title
#'
#' @param state
#' @param solution
#'
#' @return
#' @export
#'
#' @examples
find_least_distance <- function(state, solution)
{
  distance <- solution$length[1] + 1
  for (n in 1:nrow(solution)) {
    current_test_state <- c(solution$j1_state[n],
                            solution$j2_state[n],
                            solution$j3_state[n])
    if (isTRUE(all.equal(state, current_test_state))) {
      temp_distance <- solution$distance[n]
      if (temp_distance < distance) {
        distance <- temp_distance
      }
    }
  }
  return(distance)
}

