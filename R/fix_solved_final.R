#' Title
#'
#' @param cleaned_jos_resp
#' @param solved
#'
#' @return
#' @export
#'
#' @examples
fix_solved_final <- function(cleaned_jos_resp, solved)
{
  if (solved == 1) {
    final_index <- max(which(!is.na(cleaned_jos_resp)))
    cleaned_jos_resp[[final_index]] <- cleaned_jos_resp[[final_index-1]]
  }
  return(cleaned_jos_resp)
}
