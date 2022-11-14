
#' take a successful solution and add it to the item table, but will be trimmed
#' later from the item table by clean-item-table
#'
#' @param branch_table
#' @param move_counter
#'
#' @return
#' @export


finalize_wj_solution <- function(branch_table, move_counter) {

  solution_length <- move_counter-1
  move_index <- length(item_table) - length(branch_table)
  set(item_table, i = nrow(item_table), j = 'length',
      value = solution_length)

  # if current solution is less/equal to best so far, add it to the item table
  if (solution_length <= min(item_table$length, na.rm=TRUE)) {

    for (s in 1:(solution_length*2)) {
      set(item_table,
          i = nrow(item_table),
          j = as.integer(s+move_index),
          value = branch_table[[s]])
    }

    item_table <<- dplyr::add_row(item_table)  # add empty row to end of item table
  }
}

