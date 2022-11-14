
#' Title
#'
#' @param jos_states
#' @param solution
#' @param jos_cols
#'
#' @return
#' @export
#'
#' @examples
compute_judge_dist <- function(jos_states, solution, jos_cols)
{
  jos_dist <- data.frame(matrix(nrow=1, ncol=length(jos_cols)))
  colnames(jos_dist) <- jos_cols
  colnames(jos_dist) <- gsub(pattern = ".resp",
                             replacement = ".dist",
                             x = names(jos_dist))
  for (j in 1:ncol(jos_dist)) {  # for each judgment
    if (!is.na(jos_states[[j]])) {
      current_state <- unlist(jos_states[[j]])
      jos_dist[[j]] <- find_least_distance(current_state, solution)
    }
  }
  if (solution$length[1] == 0) {  # if unsolvable
    jos_dist[1:ncol(jos_dist)] <- NA
  }
  return(jos_dist)
}

