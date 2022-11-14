
#' Prep data and demographics
#'
#' Run this function to select the source folder for your data.
#' It will:
#' -create a folder for each participant
#' -copy the data file for that participant into that folder
#' -truncate the copied filename
#' -return a data frame of participants, demographics, and conditions
#'
#' @return Nothing.
#' @export



participant_list <- function()
{

  library(tools)

  # set working directory
  parent_dir <- choose.dir(getwd(), "Browse to the raw data folder")
  setwd(parent_dir)

  # determine files in data folder
  files <- list.files(path = parent_dir, full.name = FALSE)

  # initialize data frame to hold demographics/conditions
  p <- data.frame(matrix(ncol = 0, nrow = length(files)))

  # initialize columns
  p$participant <- NA
  p$stimuli <- NA
  p$age <- NA
  p$gender <- NA

  # new folder to hold the data
  dir.create("data")

  # initialize a counter
  counter <- 1

  # loop through each file
  for (i in files) {

    # this is a goofy way of determining while indices are which values
    # - ideally would improve this

    file_name <- file_path_sans_ext(files[counter])

    # if participant number is a single digit
    sub_10 <- ifelse(substr(file_name, 10, 10) == "_", TRUE, FALSE)
    sup_99 <- ifelse(substr(file_name, 11, 11) != "_", TRUE, FALSE)

    # record values
    p$stimuli[counter] <- substr(file_name, 5, 5)

    p$participant[counter] <- ifelse(sub_10 == TRUE,
                                     substr(file_name, 9, 9),
                                     ifelse(sup_99 == FALSE,
                                            substr(file_name, 9, 10),
                                            substr(file_name, 9, 11)))

    p$age[counter] <- ifelse(sub_10 == TRUE,
                             substr(file_name, 11, 12),
                             ifelse(sup_99 == FALSE,
                                    substr(file_name, 12, 13),
                                    substr(file_name, 13, 14)))

    p$gender[counter] <- ifelse(sub_10 == TRUE,
                                substr(file_name, 14, 14),
                                ifelse(sup_99 == FALSE,
                                substr(file_name, 15, 15),
                                substr(file_name, 16, 16)))

    # create a folder for this participant number
    dir.create(paste(parent_dir, "\\data\\", as.character(p$participant[counter]), sep=""))

    # set target directory to write new files
    target_dir <- paste(parent_dir, "\\data\\", p$participant[counter], sep="")

    # copy file to folder with better file name
    file.copy(paste(parent_dir, "\\", file_name, ".csv", sep=""), target_dir)

    # rename that file
    current_file <- list.files(path = target_dir, full.name = TRUE)
    file.rename(current_file, paste(target_dir, "/", p$participant[counter], ".csv", sep=""))

    # update counter
    counter <- counter + 1
  }

  # change gender to digits: male = 1, female = 0
  p$gender[p$gender %in% "F"] <- 0
  p$gender[p$gender %in% "f"] <- 0
  p$gender[p$gender %in% "M"] <- 1
  p$gender[p$gender %in% "m"] <- 1

  # change stimuli to 1 and 2 instead of a and b
  p$stimuli[p$stimuli %in% "a"] <- 1
  p$stimuli[p$stimuli %in% "b"] <- 2

  # set values to numeric
  p$participant <- as.numeric(p$participant)
  p$stimuli <- as.numeric(p$stimuli)
  p$age <- as.numeric(p$age)
  p$gender <- as.numeric(p$gender)

  # sort the list
  p_list <- p[order(p$participant),]

  # write the list to file
  write.table(p_list, paste(parent_dir, "\\data\\p_list.csv", sep=""), row.names = FALSE, sep = ",")

  return(TRUE)
}

















