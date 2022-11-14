
#' Title
#'
#' @param judges
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
clean_jos_resp <- function(judges, jos_cols)
{
  jos_resp <- judges %>%
    select(grep("Response", names(judges)))
  colnames(jos_resp) <- jos_cols
  return(jos_resp)
}
