
#' Title
#'
#' @param judges
#'
#' @return
#' @export
#'
#' @examples
create_jos_cols <- function(judges)
{
  number_of_judges <- (ncol(judges) / 3)
  judge_cols <- c()
  for (judge in 1:number_of_judges) {
    judge_cols <- c(judge_cols, paste0("j", judge-1, ".resp"))
  }
  return(judge_cols)
}
