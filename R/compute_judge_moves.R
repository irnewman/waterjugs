#' Title
#'
#' @param jos_rt
#' @param move_rt
#' @param move_states
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
compute_judge_moves <- function(jos_rt, move_rt, move_states, jos_cols)
{
  jos_mov <- data.frame(matrix(nrow=1, ncol=length(jos_cols)))
  colnames(jos_mov) <- jos_cols
  colnames(jos_mov) <- gsub(pattern = ".resp",
                            replacement = ".mov",
                            x = names(jos_mov))
  jos_jmv <- jos_mov
  colnames(jos_jmv) <-  gsub(pattern = ".mov",
                             replacement = ".jmov",
                             x = names(jos_jmv))
  for (j in 1:ncol(jos_mov)) {  # for each judgment
    if (j == 1) {
      jos_mov[[j]] <- 0
      jos_jmv[[j]] <- 0
    } else if (!is.na(jos_rt[[j]])) {  # handle NA
      j_rt <- jos_rt[[j]]
      m_rt <- max(which(move_rt < j_rt))
      jos_mov[[j]] <- m_rt
      jos_jmv[[j]] <- m_rt - jos_mov[[j-1]]
    } else {
      jos_mov[[j]] <- NA
      jos_jmv[[j]] <- NA
    }
  }
  jos_moves <- data.frame(jos_mov, jos_jmv)
  return(jos_moves)
}
