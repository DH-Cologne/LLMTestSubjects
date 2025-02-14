# analyses average performence of answers from LLMs on sentences with ditransitive verbs (A1) and state verbs (A2) #nolint

rm(list=ls())
source("99_Utils.R")

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

folder_path <- "LLMAnswers/Completions"
# iterate through subdirectories
sub_dirs <- list.dirs(folder_path, recursive = FALSE)

# find all a1 files
paths_a1 <- list.files(path = sub_dirs, pattern = "^A1_VP01_answers\\.csv$", full.names = TRUE, recursive = TRUE) #nolint
# find all a2 files
paths_a2 <- list.files(path = sub_dirs, pattern = "^A2_1_1_answers\\.csv$", full.names = TRUE, recursive = TRUE) #nolint

# datasets a1 w. action verbs
datasets_a1 <- setNames(paths_a1, basename(dirname(paths_a1)))
# datasets a2 w. state verbs
datasets_a2 <- setNames(paths_a2, basename(dirname(paths_a2)))

# ----------- INDIVIDUAL MODELS ------------

column_sums_a1 <- list()
averages_a1 <- list()

# iterate through datasets_a1
for (model_name in names(datasets_a1)) {

  # get file path
  file_path <- datasets_a1[[model_name]]

  # read file
  raw_data <- read_lines(file_path)

  # split each line w. tab
  split_data <- str_split(raw_data, pattern = "\t", simplify = TRUE)

  # convert to dataframe
  df <- as.data.frame(split_data, stringsAsFactors = FALSE)

  # convert last column to numeric
  last_col_index <- ncol(df)
  df[[last_col_index]] <- as.numeric(df[[last_col_index]])

  # occurrences of "1" in the last column
  count_ones <- sum(df[[last_col_index]] == 1, na.rm = TRUE)

  # get number of rows 
  total_rows <- nrow(df)

  # calc average
  averages <- ifelse(total_rows > 0, count_ones / total_rows, NA)

  # store in list
  column_sums_a1[[model_name]] <- count_ones
  averages_a1[[model_name]] <- averages

}

# convert lists to dataframe
results_df_a1 <- data.frame(
  Model = names(averages_a1),
  Sum = unlist(column_sums_a1),
  Averages = unlist(averages_a1)
)
# check if works as expected
# print(results_df_a1)

# plot bar chart
p1 <- ggplot(results_df_a1, aes(x = reorder(Model, Averages), y = Averages, fill = Model)) + #nolint
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Scores by Model",
       x = "Model",
       y = "Average correct identified items") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# save bar chart in file
# ggsave("Pronomina/Plots/Completions_Average_A1.png", plot = p1, width = 10, height = 6, dpi = 300) #nolint
# print bar chart
# print(p1)

# ------- A2 --------

column_sums_a2 <- list()
averages_a2 <- list()

# iterate through datasets_a2
for (model_name in names(datasets_a2)) {

  # get file path
  file_path <- datasets_a2[[model_name]]

  # read file
  raw_data <- read_lines(file_path)

  # split each line w. tab
  split_data <- str_split(raw_data, pattern = "\t", simplify = TRUE)

  # convert to dataframe
  df <- as.data.frame(split_data, stringsAsFactors = FALSE)

  # convert last column to numeric
  last_col_index <- ncol(df)
  df[[last_col_index]] <- as.numeric(df[[last_col_index]])

  # occurrences of "1" in the last column
  count_ones <- sum(df[[last_col_index]] == 1, na.rm = TRUE)

  # Get number of rows **from df, not datasets_a1**
  total_rows <- 48

  # calc average
  averages <- ifelse(total_rows > 0, count_ones / total_rows, NA)

  # store in lists
  column_sums_a2[[model_name]] <- count_ones
  averages_a2[[model_name]] <- averages

}

# convert lists to a dataframe
results_df_a2 <- data.frame(
  Model = names(averages_a2),
  Sum = unlist(column_sums_a2),
  Averages = unlist(averages_a2)
)

# plot the bar chart
p2 <- ggplot(results_df_a2, aes(x = reorder(Model, Averages), y = Averages, fill = Model)) + # nolint
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Scores by Model",
       x = "Model",
       y = "Average correct identified items") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ggsave("Pronomina/Plots/Completions_Average_A2.png", plot = p2, width = 10, height = 6, dpi = 300) #nolint
# print(p2)

# --------- ERROR ANALYSIS -----------

# good_datasets_a1 <- list(
#  "MISTRALNEMO" = file.path(folder_path, "MISTRALNEMO_12b_t02_comp/A1_VP01_answers.csv"), # nolint
#  "GPT4" = file.path(folder_path, "GPT4_t05_comp/A1_VP01_answers.csv"), # nolint
# "MIX" = file.path(folder_path, "MIX_8x7b_t02_comp/A1_VP01_answers.csv"), # nolint
#  "ML3.2" = file.path(folder_path, "ML3.2_3b_t02_comp/A1_VP01_answers.csv"), # nolint
#  "PHI3" = file.path(folder_path, "PHI3_14b_t02_comp/A1_VP01_answers.csv"), # nolint
#  "SKGEMMA2" = file.path(folder_path, "SKGEMMA2_9b_t02_comp/A1_VP01_answers.csv"), # nolint
#  "SKv2" = file.path(folder_path, "SKv2_14b_t02_comp/A1_VP01_answers.csv") # nolint
#)

#good_datasets_a2 <- list(
#  "GPT4_ExpA2" = file.path(folder_path,"GPT4_t05_comp/A2_1_1_answers.csv"), # nolint
#  "SKGEMMA2_ExpA2" = file.path(folder_path,"SKGEMMA2_9b_t02_comp/A2_1_1_answers.csv"), # nolint
#  "MIX_ExpA2" = file.path(folder_path,"MIX_8x7b_t02_comp/A2_1_1_answers.csv"), # nolint
#  "SKv2_ExpA2" = file.path(folder_path,"SKv2_14b_t02_comp/A2_1_1_answers.csv"), # nolint
#  "MISTRALNEMO_ExpA2" = file.path(folder_path,"MISTRALNEMO_12b_t02_comp/A2_1_1_answers.csv"), # nolint
#  "KFK3_ExpA2" = file.path(folder_path,"KFK3_8b_t02_comp/A2_1_1_answers.csv"), # nolint
#  "PHI3_ExpA2" = file.path(folder_path,"PHI3_14b_t02_comp/A2_1_1_answers.csv") # nolint
#)

# --------- A1 ----------

model_names_a1 <- character()
error_types_a1 <- character()
error_counts_a1 <- numeric()
error1_count_a1 <- list()
error2_count_a1 <- list()

for (model_name in names(datasets_a1)) {

  # get file path
  file_path <- datasets_a1[[model_name]]

  # read file
  raw_data <- read_lines(file_path)

  # split each line w. tab
  split_data <- str_split(raw_data, pattern = "\t", simplify = TRUE)

  # convert to dataframe
  df <- as.data.frame(split_data, stringsAsFactors = FALSE)

  # convert last column to numeric
  last_col_index <- ncol(df)

  # convert last column to numeric
  df[[last_col_index]] <- as.numeric(as.character(df[[last_col_index]]))

  # count occurrences of 0 (Error1) and 2 (Error2)
  error1_count_a1 <- sum(df[[last_col_index]] == 0, na.rm = TRUE)
  error2_count_a1 <- sum(df[[last_col_index]] == 2, na.rm = TRUE)

  # append model name, error types, and counts
  model_names_a1 <- c(model_names_a1, rep(model_name, 2))
  error_types_a1 <- c(error_types_a1, "Error1", "Error2")
  error_counts_a1 <- c(error_counts_a1, error1_count_a1, error2_count_a1)
}

error_data_a1 <- data.frame(
  Model = model_names_a1,
  ErrorType = error_types_a1,
  Count = error_counts_a1
)

# Verify the result
# print(error_data_a1)

p3 <- ggplot(error_data_a1, aes(x = reorder(Model, Count), y = Count / 36 * 100, fill = ErrorType)) + #nolint
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(title = "Error Analysis per Model",
       x = "Model",
       y = "Error Percentage (%)") +
  theme_classic() +
  theme(
    legend.position = "top",  # Move legend to top
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 13),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed(ratio = 0.05) +
  scale_fill_manual(
    values = c("Error1" = "#a249eb", "Error2" = "#05053c"),
    labels = c("Error1" = "Incorrect Continuation in Task 1", "Error2" = "Semantic Inconsistencies in Task 2") # nolint
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), labels = scales::percent_format(scale = 1)) + # nolint
  theme(legend.text = element_text(size = 12)) + coord_flip()

# Save plot
# ggsave("Pronomina/Plots/Completions_Errors_A1.png", p3, width = 8, height = 5) #nolint

print(p3)

# ----------- A2 -----------

model_names_a2 <- character()
error_types_a2 <- character()
error_counts_a2 <- numeric()
error1_count_a2 <- list()
error2_count_a2 <- list()

for (model_name in names(datasets_a2)) {

  # get file path
  file_path <- datasets_a2[[model_name]]

  # read file
  raw_data <- read_lines(file_path)

  # split each line w. tab
  split_data <- str_split(raw_data, pattern = "\t", simplify = TRUE)

  # convert to dataframe
  df <- as.data.frame(split_data, stringsAsFactors = FALSE)

  # convert last column to numeric
  last_col_index <- ncol(df)

  # convert last column to numeric
  df[[last_col_index]] <- as.numeric(as.character(df[[last_col_index]]))

  # count occurrences of 0 (Error1) and 2 (Error2)
  error1_count_a2 <- sum(df[[last_col_index]] == 0, na.rm = TRUE)
  error2_count_a2 <- sum(df[[last_col_index]] == 2, na.rm = TRUE)

  # append model name, error types, and counts
  model_names_a2 <- c(model_names_a2, rep(model_name, 2))
  error_types_a2 <- c(error_types_a2, "Error1", "Error2")
  error_counts_a2 <- c(error_counts_a2, error1_count_a2, error2_count_a2)
}

error_data_a2 <- data.frame(
  Model = model_names_a2,
  ErrorType = error_types_a2,
  Count = error_counts_a2
)


# Beide DataFrames zusammenführen und Count-Werte summieren
error_data_combined <- bind_rows(error_data_a1, error_data_a2) %>%
  group_by(Model, ErrorType) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Ergebnis anzeigen
print(error_data_combined)


#Kick out unused models
error_data_combined <- error_data_combined %>% filter(Model != "GPT4_t05_comp")
#error_data_a2 <- error_data_a2 %>% filter(Model != "EUROLLM_t02_comp")
error_data_combined <- error_data_combined %>% mutate(Model = gsub("_t02_comp", "", Model))


# verify result
# print(error_data_a2)

p4 <- ggplot(error_data_combined, aes(x = reorder(Model, Count), y = Count / (36+48) * 100, fill = ErrorType)) + #nolint
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(x = NULL, y = "Model Mistakes") + 
  theme_classic() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",  
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed(ratio = 0.05) +
  scale_fill_manual(
    values = c("Error1" = "lightblue", "Error2" = "blue"),
    labels = c("Error1" = "Incorrect Continuation in Task 1", "Error2" = "Semantic Inconsistencies in Task 2") # nolint
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), labels = scales::percent_format(scale = 1)) + # nolint
  theme(legend.text = element_text(size = 9)) +  
  coord_flip()

print(p4)

  

# ------------ ALL MODELS ------------

# read & combine all a1 files, adding the model name as a new column at the beginning #nolint
combined_data_a1 <- bind_rows(
  lapply(names(datasets_a1), function(model_name) {
    # get file path for each model
    file_path <- datasets_a1[[model_name]]

    # extract model name from the path (folder name of file)
    model_name_extracted <- str_extract(basename(dirname(file_path)), "^[^/]+")

    # read data from the file
    raw_data <- read_lines(file_path)

    # split data at tab
    split_data <- str_split(raw_data, pattern = "[\t]")

    # convert to data frame
    df <- as.data.frame(do.call(rbind, split_data))

    # add model name as new column at beginning
    df$model <- model_name_extracted
    df <- df %>% select(model, everything())  # move to first column

    return(df)
  })
)

# make sure that last col is type int so we can calculate
combined_data_a1$V6 <- as.numeric(combined_data_a1$V6)
binary_sums_a1 <- sum(combined_data_a1$V6) # sum all 1's up
total_rows_a1 <- nrow(combined_data_a1) # num of all rows to divide by

# calc ratio of correct / total
average_performance_a1 <- binary_sums_a1 / total_rows_a1
# print(average_performance_a1)

# process each file in datasets_a2 (but only first 48 rows per file)
combined_data_a2 <- bind_rows(
  lapply(names(datasets_a2), function(model_name) {
    # file path for each model
    file_path <- datasets_a2[[model_name]]

    # extract model name from the path (folder name of file)
    model_name_extracted <- str_extract(basename(dirname(file_path)), "^[^/]+")

    # read from file
    raw_data <- read_lines(file_path)

    # split data at tab
    split_data <- str_split(raw_data, pattern = "[\t]")

    # convert to data frame
    df <- as.data.frame(do.call(rbind, split_data))

    # filter only first 48 rows
    df <- df[1:48, ]

    # add the model name as a new column at the beginning
    df$model <- model_name_extracted
    df <- df %>% select(model, everything())  # "model" @ first column

    return(df)
  })
)

# make sure that last col is type int so we can calculate
combined_data_a2$V6 <- as.numeric(combined_data_a2$V6)
binary_sums_a2 <- sum(combined_data_a2$V6) # sum all 1's up
total_rows_a2 <- nrow(combined_data_a2) # num of all rows to divide by

# calc ratio of correct / total
average_performance_a2 <- binary_sums_a2 / total_rows_a2
# print(average_performance_a2)

# Write the combined cleaned file for A2
# write_csv(combined_data_a1, "Pronomina/LLMAnswers/Completions/combined_cleaned_A1.csv") #nolint
# write_csv(combined_data_a2, "Pronomina/LLMAnswers/Completions/combined_cleaned_A2.csv") #nolint

# data <- readRDS("/Users/lea/Desktop/LLM_test_subjects/Pronomina/Data/ExpB1DataAnswers.rds") #nolint
# write.csv(data, "/Users/lea/Desktop/EXP_LLM/output.csv", row.names = FALSE)
