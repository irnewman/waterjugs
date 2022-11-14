#' Title
#'
#' @param trial
#' @param solution
#' @param trial_info
#'
#' @return
#' @export
#'
#' @examples
clean_wj_trial <- function(trial, solution, trial_info)
{
  # compute start, goal, jug capacities
  start_state <- c(trial_info$Jug1Start,
                   trial_info$Jug2Start,
                   trial_info$Jug3Start)
  goal_state <- c(trial_info$Jug1Goal,
                  trial_info$Jug2Goal,
                  trial_info$Jug3Goal)
  jug_caps <- c(trial_info$Jug1Capacity,
                trial_info$Jug2Capacity,
                trial_info$Jug3Capacity)
  # clean moves
  moves <- select_moves(trial)
  cleaned_move_states <- clean_move_states(start_state, moves, jug_caps)
  cleaned_move_rt <- clean_move_rt(moves)
  # clean judges
  judges <- select_judges(trial)
  jos_cols <- create_jos_cols(judges)
  cleaned_jos_resp <- clean_jos_resp(judges, jos_cols)
  cleaned_jos_rt <- clean_jos_rt(judges, jos_cols)
  cleaned_jos_jrt <- clean_jos_jrt(judges, jos_cols)
  # compute judge variables
  cleaned_jos_state <- compute_judge_states(cleaned_jos_rt,
                                            cleaned_move_rt,
                                            cleaned_move_states,
                                            start_state,
                                            jos_cols)
  cleaned_jos_dist <- compute_judge_dist(cleaned_jos_state,
                                         solution,
                                         jos_cols)
  cleaned_jos_moves <- compute_judge_moves(cleaned_jos_rt,
                                           cleaned_move_rt,
                                           cleaned_move_states,
                                           jos_cols)

  # compute variables
  rt <- max(cleaned_jos_rt, na.rm=TRUE)
  first_jos <- cleaned_jos_resp[[1]]

  last_state_index <- max(which(!is.na(cleaned_jos_state)))
  last_state <- as.numeric(unlist(cleaned_jos_state[[last_state_index]]))
  solved <- as.numeric(goal_reached(last_state, goal_state))

  final_jos <- ifelse(solved == 0,
                      cleaned_jos_resp[[last_state_index]],
                      cleaned_jos_resp[[last_state_index-1]])

  cleaned_jos_resp <- fix_solved_final(cleaned_jos_resp, solved)


  cleaned_jos_data <- data.frame(cleaned_jos_resp,
                                 cleaned_jos_rt,
                                 cleaned_jos_jrt,
                                 cleaned_jos_state,
                                 cleaned_jos_dist,
                                 cleaned_jos_moves)
  cleaned_info <- data.frame(trial_info[1:9], trial_info[ncol(trial_info)])
  cleaned_vars <- data.frame(rt, solved, first_jos, final_jos)
  cleaned_trial <- cbind(cleaned_info, cleaned_vars, cleaned_jos_data)
  return(cleaned_trial)
}
