

# WARNINGS HERE
#' Title
#'
#' @param jos_rt
#' @param move_rt
#' @param move_states
#' @param start_state
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
compute_judge_states <- function(jos_rt, move_rt, move_states, start_state,
                                 jos_cols)
{
  jos_state <- data.frame(matrix(nrow=1, ncol=length(jos_cols)))
  colnames(jos_state) <- jos_cols
  colnames(jos_state) <- gsub(pattern = ".resp",
                              replacement = ".state",
                              x = names(jos_state))
  for (j in 1:ncol(jos_state)) {  # for each judgment
    if (j == 1) {
      jos_state[[j]] <- list(start_state)
    } else if (!is.na(jos_rt[[j]])) {  # handle NA
      j_rt <- jos_rt[[j]]
      if (1 > length(which(move_rt < j_rt))) {  # handles no moves
        #print("compute_judge_states: no moves")
        jos_state[[j]] <- jos_state[[j-1]]
      } else {
        m_rt <- max(which(move_rt < j_rt), na.rm=TRUE)
        jos_state[[j]] <- move_states[[m_rt]]
      }
    } else {
      jos_state[[j]] <- NA
    }
  }
  return(jos_state)
}
