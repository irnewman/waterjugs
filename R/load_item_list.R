
#' Title
#'
#' @param filename
#' @param directory
#'
#' @return
#' @export
#'

load_item_list <- function(filename, directory)
{
  loaded_list <- readr::read_csv(paste0(directory, "//", filename))

  item_list <- data.frame(matrix(nrow = nrow(loaded_list), ncol = 6))
  colnames(item_list) <- c("start_values", "goal_values", "cap_values",
                           "length", "first_moves", "solution")


  for (r in 1:nrow(item_list)) {

    item_list$start_values[r] <- list(c(loaded_list$s1[r],
                                        loaded_list$s2[r],
                                        loaded_list$s3[r]))
    item_list$goal_values[r] <- list(c(loaded_list$g1[r],
                                       loaded_list$g2[r],
                                       loaded_list$g3[r]))
    item_list$cap_values[r] <- list(c(loaded_list$c1[r],
                                      loaded_list$c2[r],
                                      loaded_list$c3[r]))
  }
  return(item_list)
}
