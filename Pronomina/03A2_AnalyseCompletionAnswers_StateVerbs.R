rm(list=ls())

# Function to extract the last word of a string
extractLastWord <- function(s) {
  # Regular expression that removes everything except the last word
  lastWord <- sub(".*\\s(\\w+)[^\\w]*$", "\\1", s)
  return(lastWord)
}

# function to shorten words to 5 letters (to treat german dative inflection)
shorten_words <- function(x) {
  sapply(x, function(cell) {
    if (nzchar(cell)) { # check if cell is empty
      words <- strsplit(cell, " ")[[1]]
      short_words <- sapply(words, function(word) substr(word, 1, 5))
      paste(short_words, collapse=" ")
    } else {
      "" # for empty cells
    }
  })
}

# Read original Dataframe (replace with "Data/ExpA2DataAnswers.rds" to analyse A2 data)
expAAnswers <- readRDS("Data/ExpA2DataAnswers.rds")
columns <- grep("Antecedens$", names(expAAnswers), value = TRUE)

# Build analysis data
condensedData <- data.frame(ID = expAAnswers$ID, 
                            RE1 = extractLastWord(expAAnswers$RE1), 
                            RE2 = extractLastWord(expAAnswers$RE2),
                            expAAnswers[, columns])

cols_to_modify <- setdiff(names(condensedData), "ID")

for (col in cols_to_modify) {
  condensedData[[col]] <- shorten_words(condensedData[[col]])
}

# grep all columns that end with "Antecedens"
antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)


# function to replace Antecedences with pre-defined role from original file
replace_values <- function(row) {
  for (col in antecedens_columns) {
    value <- row[[col]]
    if (value == row[["RE1"]] || value == row[["RE2"]]) {
      row[[col]] <- ifelse(value == row[["RE1"]], "RE1", "RE2")
    } else {
      row[[col]] <- NA
    }
  }
  return(row)
}

# replace Antecedences
condensedData <- as.data.frame(t(apply(condensedData, 1, replace_values)))

# Antecedences to factors
condensedData[antecedens_columns] <- lapply(condensedData[antecedens_columns], factor)
condensedData$Human_Antecedens <- expAAnswers$AntecedentAnswer
# Replace: NP1 -> RE1, NP2 -> RE2, other -> NA
condensedData$Human_Antecedens <- ifelse(condensedData$Human_Antecedens == "NP1", "RE1",
                                            ifelse(condensedData$Human_Antecedens == "NP2", "RE2", NA))
condensedData$Human_Antecedens <- as.factor(condensedData$Human_Anteceden)
antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)


condensedData$AO = expAAnswers$ArgumentOrder
condensedData$REP = as.factor(expAAnswers$REP)
condensedData$ItemNumber = expAAnswers$ItemNumber
summary(condensedData)
names(condensedData)



# fuction to count RE1 and RE2 as Antecedences
count_frequencies <- function(x) {
  c(RE1 = sum(x == "RE1", na.rm = TRUE),
    RE2 = sum(x == "RE2", na.rm = TRUE),
    NA_count = sum(is.na(x)))
}

# Aggregate data by ArgumentOrder (OS vs. SO) and Replacement-String (der vs. dieser vs. er)
aggregated_data <- aggregate(condensedData[, antecedens_columns], 
                             by = list(AO = condensedData$AO, REP = condensedData$REP), #ItemNumber = condensedData$ItemNumber), 
                             FUN = count_frequencies)
result <- do.call(data.frame, aggregated_data)


# Calculate differences between counts of RE1 and RE2
calculate_difference <- function(x) {
  sum(x == "RE1", na.rm = TRUE) - sum(x == "RE2", na.rm = TRUE)
}

# Calculate relative differences between counts of RE1 and RE2
calculate_relative_difference <- function(x) {
  re1_count <- sum(x == "RE1", na.rm = TRUE)
  re2_count <- sum(x == "RE2", na.rm = TRUE)
  if (re1_count + re2_count > 0) {
    (re1_count - re2_count) / (re1_count + re2_count)
  } else {
    NA  # if no RE1 and RE2 are found
  }
}

# Aggregate data by ArgumentOrder (OS vs. SO) and Replacement-String (der vs. dieser vs. er)
preferences_data <- aggregate(condensedData[, antecedens_columns], 
                             by = list(AO = condensedData$AO, REP = condensedData$REP), 
                             FUN = calculate_difference)
result <- do.call(data.frame, preferences_data)

# plot preferences
par(mfrow = c(4, 4))
plot_columns <- names(result)[4:length(result)]


for (col in plot_columns) {
  bar_colors <- ifelse(result$AO == "OS", "lightblue", "darkblue")
  barplot(result[[col]], main = col, xlab = "Groups", ylab = "Rel.Diff", col = bar_colors)
  #legend("bottomright", legend = c("OS", "SO"), fill = c("lightblue", "darkblue"), bty = "n")
}


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

summary(aggregated_df)

par(mfrow = c(3, 3))

# Erstellen von Balkendiagrammen für jede Antecedens-Spalte


