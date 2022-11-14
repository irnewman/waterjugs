#' Title
#'
#' @param trial
#'
#' @return
#' @export
#'
#' @examples
select_judges <- function(trial)
{
  first_col <- "Judgment1DisplayedTime"
  last_col <- "Move1Jug1"  # N - 1
  first_index <- match(first_col, names(trial))
  last_index <- match(last_col, names(trial)) - 1
  if (is.na(last_index)) {
    last_index <- ncol(trial)
  }
  judges <- trial[first_index:last_index]
  return(judges)
}
