#' Initialize Item and Branch Tables Needed to Compute Solutions
#'
#' 1 of 3 helper functions, creates the item table for current item
#'
#'
#'
#' @param max_tries User defined maximum tries per solution branch.
#' @param moves_only Set true for branch table.
#'
#' @return An initialized table.
#'
#' @return
#' @export


initialize_item_table <- function(max_tries, moves_only=FALSE) {

  # vector of move columns, based on maximum moves allowed
  move_columns <- c()
  n <- 1
  for (k in 1:(max_tries*2)) {
    if (k%%2 == 1) {
      current_column <- paste("m", n, "j1", sep="")
    }
    else {
      current_column <- paste("m", n, "j2", sep="")
      n <- n+1
    }
    move_columns <- cbind(move_columns, current_column)
  }

  # create table
  table_columns <- c("item", "length", "start_moves",
                     "j1cap", "j2cap", "j3cap",
                     "j1start", "j2start", "j3start",
                     "j1goal", "j2goal", "j3goal")

  # set columns for table
  if (moves_only) {
    return_columns <- c(move_columns)
  } else {
    return_columns <- c(table_columns, move_columns)
  }


  return_table <- data.table(matrix(nrow=1, ncol=length(return_columns)))
  colnames(return_table) <- return_columns

  # set values to numeric
  for (p in 1:ncol(return_table)) {
    return_table[[p]] <- as.numeric(return_table[[p]])
  }

  return(return_table)

}
