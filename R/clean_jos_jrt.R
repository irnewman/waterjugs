#' Title
#'
#' @param judges
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
clean_jos_jrt <- function(judges, jos_cols)
{
  jos_shown <- judges %>%
    select(grep("Displayed", names(judges)))
  jos_entered <- judges %>%
    select(grep("Submitted", names(judges)))
  jos_jrt <- data.frame(matrix(nrow=1, ncol=length(jos_cols)))
  colnames(jos_jrt) <- jos_cols
  colnames(jos_jrt) <- gsub(pattern = ".resp",
                            replacement = ".jrt",
                            x = names(jos_jrt))
  for (i in 1:ncol(jos_jrt)) {
    jos_jrt[i] <- jos_entered[i] - jos_shown[i]
  }
  return(jos_jrt)
}
