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

condensedData$PromptID = paste(expBAnswers$ContextS, expBAnswers$REP)

# Berechnen der Durchschnittswerte für jede PromptID in den rating_columns
average_ratings <- aggregate(. ~ PromptID, data = condensedData[rating_columns], FUN = function(x) mean(x, na.rm = TRUE))

# Berechnen der Durchschnittswerte für jede PromptID in den rating_columns
average_ratings <- aggregate(condensedData[rating_columns], by = list(condensedData$PromptID), FUN = function(x) mean(x, na.rm = TRUE))

# Umbenennen der Gruppenspalte für Klarheit
colnames(average_ratings)[1] <- "PromptID"

# Anzeigen der Ergebnisse
print(head(average_ratings))


# Anzeigen der Ergebnisse
print(head(average_ratings))



# Calculate a 3-dim-vector (RE1-RE2-NA) for each PromptID
result_vector <- lapply(unique(condensedData$PromptID), function(prompt_id) {
  # filter specific PromptID 
  specific_data <- condensedData[condensedData$PromptID == prompt_id, antecedens_columns]
  
  # Count RE1, RE2 und NA within each antecedens_columns
  re1_count <- sum(specific_data == "RE1", na.rm = TRUE)
  re2_count <- sum(specific_data == "RE2", na.rm = TRUE)
  na_count <- sum(is.na(specific_data))
  
  # Generate Vector
  c(RE1_count = re1_count, RE2_count = re2_count, NA_count = na_count)
})

# Vector -> DF
result_df <- do.call(rbind, result_vector)
rownames(result_df) <- unique(condensedData$PromptID)
print(result_df)

# DF for aggregated values
aggregated_df <- data.frame()

# Iterieren through antecedens_column
for (col in antecedens_columns) {
  # Aggregieren Data for each PromptID
  for (prompt_id in unique(condensedData$PromptID)) {
    specific_data <- condensedData[condensedData$PromptID == prompt_id, col]
    
    # Count RE1, RE2 und NA
    re1_count <- sum(specific_data == "RE1", na.rm = TRUE)
    re2_count <- sum(specific_data == "RE2", na.rm = TRUE)
    na_count <- sum(is.na(specific_data))
    
    # New DF
    aggregated_df <- rbind(aggregated_df, data.frame(PromptID = prompt_id, 
                                                     Column = col, 
                                                     RE1_count = re1_count, 
                                                     RE2_count = re2_count, 
                                                     NA_count = na_count))
  }
}

print(head(aggregated_df))

