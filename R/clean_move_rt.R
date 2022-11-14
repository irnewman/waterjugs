#' Title
#'
#' @param moves
#'
#' @return
#' @export
#'
#' @examples
clean_move_rt <- function(moves)
{
  number_of_moves <- (ncol(moves) / 5)
  moves_rt <- moves %>%
    select(grep("Time2", names(moves)))
  colnames(moves_rt) <- gsub(pattern = "Time2",
                             replacement = ".rt",
                             x = names(moves_rt))
  return(moves_rt)
}
