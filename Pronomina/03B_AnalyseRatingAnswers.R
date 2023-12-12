rm(list=ls())

# Function to extract the last word of a string
extractLastWord <- function(s) {
  # Regular expression that removes everything except the last word
  lastWord <- sub(".*\\s(\\w+)[^\\w]*$", "\\1", s)
  return(lastWord)
}

# Read original Dataframe
expBAnswers <- readRDS("Data/ExpBDataAnswers.rds")
columns <- grep("_Rating$", names(expBAnswers), value = TRUE)

# Build analysis data
condensedData <- data.frame(ID = expBAnswers$ID, 
                            ExperimentID = expBAnswers$ExperimentID,
                            Human_Rating = expBAnswers$Rating7,
                            expBAnswers[, columns])

#TODO Delete when EXP2 is performed
# Delete experiments not needes for the analysis
# condensedData <- subset(condensedData, !ExperimentID %in% c("B2"))

# grep all columns that end with "Antecedens"
rating_columns <- grep("_Rating$", names(condensedData), value = TRUE)

# Replace all values outside 0<x<8 by NA
replace_out_of_range <- function(column) {
  column[!(column %in% 1:7)] <- NA
  return(column)
}
condensedData[rating_columns] <-lapply(condensedData[rating_columns], replace_out_of_range)
condensedData[rating_columns] <- lapply(condensedData[rating_columns], as.numeric)


summary(condensedData)

# inter rater library
library(irr)

condensedData$PromptID = paste(expBAnswers$ContextS, substr(expBAnswers$TargetPrompt, 1, nchar(expBAnswers$TargetPrompt)-51))


# Calculate mean for each PromptID for each model
average_ratings <- aggregate(condensedData[rating_columns], by = list(condensedData$PromptID), FUN = function(x) mean(x, na.rm = TRUE))
colnames(average_ratings)[1] <- "PromptID"

summary(average_ratings)


