#' Creates required tables to compute solutions.
#'
#' Computing the solutions requires 3 tables, which are created with this
#' function through 3 helper functions:
#' -branch_table: the moves for a single branch of the current solution attempt
#' -item_table: all the solutions of all lengths for the current item
#' -solution_table: the table to store all the optimal solutions
#'
#' @return Nothing.
#' @export

create_tables <- function(max_tries)
{
  branch_table <<- initialize_item_table(max_tries, moves_only = TRUE)
  item_table <<- initialize_item_table(max_tries)
  solution_table <<- initialize_solution_table(max_tries)
}
