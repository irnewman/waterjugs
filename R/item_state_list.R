#' makes an item list from all the start/goal combinations?
#'
#' @param jug_caps
#' @param total_water
#'
#' @return
#' @export
#'

# CHANGE NAME and change in water jug stimuli script
item_state_list <- function(jug_caps, total_water)
{

  start_list <- start_state_calc(jug_caps, total_water)
  goal_list <- goal_state_calc(jug_caps, total_water)

  item_list <- data.frame(matrix(nrow = nrow(start_list) * nrow(goal_list),
                                 ncol = 6))
  colnames(item_list) <- c("start_values", "goal_values", "cap_values",
                           "length", "first_moves", "solution")

  line_counter <- 1
  for (a in 1:nrow(start_list)) {
    for (b in 1:nrow(goal_list)) {

      s <- c(start_list[a,1], start_list[a,2], start_list[a,3])
      g <- c(goal_list[b,1], goal_list[b,2], goal_list[b,3])

      if (!(all(s == g))) {
        item_list$start_values[line_counter] <- list(c(start_list[a,1],
                                                       start_list[a,2],
                                                       start_list[a,3]))
        item_list$goal_values[line_counter] <- list(c(goal_list[b,1],
                                                      goal_list[b,2],
                                                      goal_list[b,3]))
        item_list$cap_values[line_counter] <- list(c(jug_caps[1],
                                                     jug_caps[2],
                                                     jug_caps[3]))
        item_list$solution[line_counter] <- list(c())
      }
      line_counter <- line_counter + 1
    }
  }

  rows_to_remove <- is.na(item_list$start_values)
  item_list <- item_list[!rows_to_remove,]
}
