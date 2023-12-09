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

# Read original Dataframe
expAAnswers <- readRDS("Data/expADataAnswers.rds")
columns <- grep("Antecedens$", names(expAAnswers), value = TRUE)

# Build analysis data
condensedData <- data.frame(expAAnswers$ID, 
                            RE1 = extractLastWord(expAAnswers$RE1), 
                            RE2 =extractLastWord(expAAnswers$RE2),
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
summary(condensedData)

# inter rater library
library(irr)

# Calculate pairwise Cohen's kappa
results <- data.frame()
for (i in 1:(length(antecedens_columns) - 1)) {
  for (j in (i + 1):length(antecedens_columns)) {
    column1 <- antecedens_columns[i]
    column2 <- antecedens_columns[j]
    kappa_value <- kappa2(condensedData[, c(column1, column2)])$value
    results <- rbind(results, data.frame(Column1 = column1, Column2 = column2, Kappa = kappa_value))
  }
}
print(results)
