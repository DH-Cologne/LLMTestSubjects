
# Analyses of accuracy of answers from LLMs on sentences with ditransitive verbs (A1) and state verbs (A2) #nolint
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

folder_path <- "Pronomina/LLMAnswers/Completions"

# datasets a1 w. action verbs
datasets_a1 <- list(
  "EUROLLM_ExpA1" = file.path(folder_path, "EUROLLM_9b_t02_comp/A1_VP01_answers.csv"), # nolint
  "GPT4_ExpA1" = file.path(folder_path, "GPT4_t05_comp/A1_VP01_answers.csv"), # nolint
  "GRAN_ExpA1" = file.path(folder_path, "GRAN_8b_t02_comp/A1_VP01_answers.csv"), # nolint
  "KFK3_ExpA1" = file.path(folder_path, "KFK3_8b_t02_comp/A1_VP01_answers.csv"), # nolint
  "Ministral_ExpA1" = file.path(folder_path, "Ministral_8b_t02_comp/A1_VP01_answers.csv"), # nolint
  "MISTRALNEMO_ExpA1" = file.path(folder_path, "MISTRALNEMO_12b_t02_comp/A1_VP01_answers.csv"), # nolint
  "MIX_ExpA1" = file.path(folder_path, "MIX_8x7b_t02_comp/A1_VP01_answers.csv"), # nolint
  "ML3.1_ExpA1" = file.path(folder_path, "ML3.1_8b_t02_comp/A1_VP01_answers.csv"), # nolint
  "ML3.2_ExpA1" = file.path(folder_path, "ML3.2_3b_t02_comp/A1_VP01_answers.csv"), # nolint
  "PHI3_ExpA1" = file.path(folder_path, "PHI3_14b_t02_comp/A1_VP01_answers.csv"), # nolint
  "QWEN2.5_ExpA1" = file.path(folder_path, "QWEN2.5_14b_t02_comp/A1_VP01_answers.csv"), # nolint
  "SKGEMMA2_ExpA1" = file.path(folder_path, "SKGEMMA2_9b_t02_comp/A1_VP01_answers.csv"), # nolint
  "SKv2_ExpA1" = file.path(folder_path, "SKv2_14b_t02_comp/A1_VP01_answers.csv"), # nolint
  "Teuken_ExpA1" = file.path(folder_path, "Teuken-7b_t02_comp/A1_VP01_answers.csv") # nolint
)

# datasets a2 w. state verbs
datasets_a2 <- list(
    "EUROLLM_ExpA2" = file.path(folder_path, "EUROLLM_9b_t02_comp/A2_1_1_answers.csv"), # nolint
    "GPT4_ExpA2" = file.path(folder_path,"GPT4_t05_comp/A2_1_1_answers.csv"), # nolint
    "GRAN_ExpA2" = file.path(folder_path,"GRAN_8b_t02_comp/A2_1_1_answers.csv"), # nolint
    "KFK3_ExpA2" = file.path(folder_path,"KFK3_8b_t02_comp/A2_1_1_answers.csv"), # nolint
    "Ministral_ExpA2" = file.path(folder_path,"Ministral_8b_t02_comp/A2_1_1_answers.csv"), # nolint
    "MISTRALNEMO_ExpA2" = file.path(folder_path,"MISTRALNEMO_12b_t02_comp/A2_1_1_answers.csv"), # nolint
    "MIX_ExpA2" = file.path(folder_path,"MIX_8x7b_t02_comp/A2_1_1_answers.csv"), # nolint
    "ML3.1_ExpA2" = file.path(folder_path,"ML3.1_8b_t02_comp/A2_1_1_answers.csv"), # nolint
    "ML3.2_ExpA2" = file.path(folder_path,"ML3.2_3b_t02_comp/A2_1_1_answers.csv"), # nolint
    "PHI3_ExpA2" = file.path(folder_path,"PHI3_14b_t02_comp/A2_1_1_answers.csv"), # nolint
    "QWEN2.5_ExpA2" = file.path(folder_path,"QWEN2.5_14b_t02_comp/A2_1_1_answers.csv"), # nolint
    "SKGEMMA2_ExpA2" = file.path(folder_path,"SKGEMMA2_9b_t02_comp/A2_1_1_answers.csv"), # nolint
    "SKv2_ExpA2" = file.path(folder_path,"SKv2_14b_t02_comp/A2_1_1_answers.csv"), # nolint
    "Teuken_ExpA2" = file.path(folder_path,"Teuken-7b_t02_comp/A2_1_1_answers.csv") # nolint
)

# ----------- INDIVIDUAL MODELS ------------

# empty list to store sums and averages
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
# (to check if works as expected)
print(results_df_a1)

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
ggsave("Pronomina/Plots/Completions_Average_A1.png", plot = p1, width = 10, height = 6, dpi = 300) #nolint

# ------- A2 --------

# empty list to store sums and averages
column_sums_a2 <- list()
averages_a2 <- list()

# terate through datasets_a1
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

ggsave("Pronomina/Plots/Completions_Average_A2.png", plot = p2, width = 10, height = 6, dpi = 300) #nolint


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
print(average_performance_a1)

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
print(average_performance_a2)

# Write the combined cleaned file for A2
# write_csv(combined_data_a1, "Pronomina/LLMAnswers/Completions/combined_cleaned_A1.csv") #nolint
# write_csv(combined_data_a2, "Pronomina/LLMAnswers/Completions/combined_cleaned_A2.csv") #nolint