# Analyses of the completion answers from LLMs on sentences with ditransitive verbs (A1) and state verbs (A2)
rm(list=ls())
source("99_Utils.R")

# Read original Dataframes (A1: action verbs, A2: state verbs)
expAAnswers <- readRDS("Data/ExpA1DataAnswers.rds") # data for action verbs
expAAnswers <- readRDS("Data/ExpA2DataAnswers.rds") # data for state verbs
columns <- grep("Antecedens$", names(expAAnswers), value = TRUE)

# Build analysis data
condensedData <- data.frame(ID = expAAnswers$ID, 
                            RE1 = extractLastWord(expAAnswers$RE1), 
                            RE2 = extractLastWord(expAAnswers$RE2),
                            expAAnswers[, columns])

cols_to_modify <- setdiff(names(condensedData), "ID")

# condensedData<-  replace_redundantanswers(condensedData)

for (col in cols_to_modify) {
  condensedData[[col]] <- shorten_words(condensedData[[col]])
}

# grep all columns that end with "Antecedens"
antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)

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
# names(condensedData)



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
par(mfrow = c(3, 4))
plot_columns <- names(result)[4:length(result)]


for (col in plot_columns) {
  bar_colors <- ifelse(result$AO == "OS", "lightblue", "darkblue")
  barplot(result[[col]], main = col, xlab = "Groups", ylab = "Rel.Diff", col = bar_colors)
  #legend("bottomright", legend = c("OS", "SO"), fill = c("lightblue", "darkblue"), bty = "n")
}

# TODO IRR measures



