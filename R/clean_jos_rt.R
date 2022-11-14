
#' Title
#'
#' @param judges
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
clean_jos_rt <- function(judges, jos_cols)
{
  jos_rt <- judges %>%
    select(grep("Submitted", names(judges)))
  colnames(jos_rt) <- jos_cols
  colnames(jos_rt) <- gsub(pattern = ".resp",
                           replacement = ".rt",
                           x = names(jos_rt))
  return(jos_rt)
}
