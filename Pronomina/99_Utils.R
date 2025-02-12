# Set of functions that are needed in various scripts of the project

# Load necessary libraries
library(ggplot2)
library(tidyr)  # For transforming data to long format
library(dplyr)  # For data manipulation
library(philentropy) # For entropy measures

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

# Split String at n_th underscore 
split_at_nth_underscore <- function(input_string, n) {
  parts <- unlist(strsplit(input_string, "_"))
  if (length(parts) < n) {
    return(input_string) 
  }
  return(paste(parts[1:n], collapse="_"))
}


## Compute Jensen-Shannon Divergence (JSD) for columns
compute_jsd <- function(df, col1, col2) {
  # Extract probability distributions (ensure non-negative values)
  p1 <- df[[col1]]
  p2 <- df[[col2]]
  # Normalize to create probability distributions (sum to 1)
  p1 <- p1 / sum(p1, na.rm = TRUE)
  p2 <- p2 / sum(p2, na.rm = TRUE)
  # Create a matrix of the two distributions
  prob_matrix <- rbind(p1, p2)
  # Compute Jensen-Shannon Divergence
  jsd_value <- JSD(prob_matrix, unit = "log2")
  return(jsd_value)
}

## Function to identify matching PromptIDs that differ by only "Dieser" vs. "Der" or "Diese" vs. "Sie"
find_matching_pairs <- function(df) {
  # Create a modified version of PromptID without "Dieser" and "Der", also without "Diese" and "Sie"
  df <- df %>%
    mutate(PromptID_modified = gsub("\\b(Dieser|Er|Diese|Sie)\\b", "", PromptID))
  # Find duplicated modified PromptIDs (these are the pairs)
  duplicated_ids <- df$PromptID_modified[duplicated(df$PromptID_modified) | duplicated(df$PromptID_modified, fromLast = TRUE)]
  # Filter only rows that are part of a matching pair
  df_pairs <- df %>% filter(PromptID_modified %in% duplicated_ids)
  return(df_pairs)
}

