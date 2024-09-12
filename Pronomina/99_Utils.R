# Set of functions that are needed in various scripts of the project

# Load necessary libraries
library(ggplot2)
library(tidyr)  # For transforming data to long format
library(dplyr)  # For data manipulation

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

# Replace all values outside 0<x<8 by NA
replace_out_of_range <- function(column) {
  column[!(column %in% 1:7)] <- NA
  return(column)
}