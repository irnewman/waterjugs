
#' Initialize Solution Table Needed to Compile Solutions
#'
#' @param max_tries User defined maximum tries per solution branch.
#' @param moves_only Set true for branch table.
#'
#' @return An initialized table.
#' @export
#'
#' @examples


initialize_solution_table <- function(max_tries, moves_only=FALSE) {


  # create table
  return_columns <- c("length", "start_moves",
                     "j1cap", "j2cap", "j3cap",
                     "j1start", "j2start", "j3start",
                     "j1goal", "j2goal", "j3goal")
  return_table <- data.table(matrix(nrow=0, ncol=length(return_columns)))
  colnames(return_table) <- return_columns

  # set values to numeric
  for (p in 1:ncol(return_table)) {
    return_table[[p]] <- as.numeric(return_table[[p]])
  }

  return(return_table)

}
