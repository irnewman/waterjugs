
#' Solve all possible jug combinations from parameters.
#'
#' @param max_tries
#' @param capacities
#' @param total_water
#'
#' @return
#' @export
#'

solve_possible_jugs <- function(max_tries, capacities, total_water)
{

  create_tables(max_tries)
  item_list <- item_state_list(capacities, total_water)

  # timer
  timer <- proc.time()

  # fills item list with solutions for each combination
  for (d in 1:nrow(item_list)) {

    # starting and goal values from item list
    item_start <- as.numeric(unlist(item_list[[1]][d]))
    item_goal <- as.numeric(unlist(item_list[[2]][d]))

    # print counter of item in list
    print(paste(d, "of", nrow(item_list), sep=" "))

    # solve the jug values given (could also make function to take values in from user)
    solve_jug(item_start, item_goal, capacities, 1, branch_table)

    # CHANGE
    current_solution <- clean_item_table(item_table)
    current_solution$item <- d

    if (nrow(solution_table) == 0) {
      solution_table <- current_solution
    } else {
      solution_table <- rbind(solution_table, current_solution)
    }

  }
  proc.time() - timer

  parent_dir <- paste0(here::here())
  setwd(parent_dir)

  clean_table <- solution_table
  clean_table$move[clean_table$move=="NULL"] <- NA

  filename <- paste0("WJ ",
                     capacities[1], "-",
                     capacities[2], "-",
                     capacities[3],
                     " ", total_water, "-water.csv")
  fwrite(clean_table, file = paste0(parent_dir,
                                    "\\",
                                    filename), na = "")

}
