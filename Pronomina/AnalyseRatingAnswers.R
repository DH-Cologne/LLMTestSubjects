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
condensedData <- subset(condensedData, !ExperimentID %in% c("B2"))

# grep all columns that end with "Antecedens"
rating_columns <- grep("_Rating$", names(condensedData), value = TRUE)

# Funktion, um Werte, die nicht zwischen 1 und 7 liegen, durch NA zu ersetzen
replace_out_of_range <- function(column) {
  column[!(column %in% 1:7)] <- NA
  return(column)
}

# Wenden Sie diese Funktion auf alle Rating-Spalten an
condensedData[rating_columns] <- lapply(condensedData[rating_columns], replace_out_of_range)


# replace Antecedences
condensedData <- as.data.frame(t(apply(condensedData, 1, replace_values)))

# Antecedences to factors
condensedData[antecedens_columns] <- lapply(condensedData[antecedens_columns], factor)
condensedData$Expected_Antecedens = expAAnswers$AntecedentAnswer
# Replace: NP1 -> RE1, NP2 -> RE2, other -> NA
condensedData$Expected_Antecedens <- ifelse(condensedData$Expected_Antecedens == "NP1", "RE1",
                                            ifelse(condensedData$Expected_Antecedens == "NP2", "RE2", NA))

antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)
summary(condensedData)

# inter rater library
library(irr)

# Calculate pairwise Cohen's kappa
# and - if de-commented - Krippendorff's alpha, which takes a little longer...
results_kappa <- data.frame()
#results_alpha <- data.frame()
for (i in 1:(length(antecedens_columns) - 1)) {
  for (j in (i + 1):length(antecedens_columns)) {
    column1 <- antecedens_columns[i]
    column2 <- antecedens_columns[j]
    print(paste("Processing columns:", column1, "and", column2))
    
    kappa_value <- kappa2(condensedData[, c(column1, column2)])$value
    results_kappa <- rbind(results_kappa, data.frame(Column1 = column1, Column2 = column2, Kappa = kappa_value))
    
    #matrix_for_alpha <- as.matrix(condensedData[, c(column1, column2)])
    #alpha_value <- kripp.alpha(matrix_for_alpha, method="nominal")$value
    #results_alpha <- rbind(results_alpha, data.frame(Column1 = column1, Column2 = column2, Alpha = alpha_value))
  }
}
print(results_kappa)
#print(results_alpha)

condensedData$PromptID = paste0(expAAnswers$ItemNumber,expAAnswers$ArgumentOrder,expAAnswers$REP)


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

