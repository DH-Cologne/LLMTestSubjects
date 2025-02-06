rm(list=ls())
source("99_Utils.R")

## Load perplexity answers to DataFrame
base_dir <- "LLMAnswers/Perplexity"
sub_dirs <- list.dirs(base_dir, recursive = FALSE)
final_df <- data.frame(ID = character(), stringsAsFactors = FALSE)
for (sub_dir in sub_dirs) {
  tsv_files <- list.files(sub_dir, full.names = TRUE)
  for (file in tsv_files) {
    data <- read.delim(file, header = FALSE, stringsAsFactors = FALSE)
    # Ensure the file has at least 3 columns before processing
    if (ncol(data) < 3) next
    # Extract columns: IDs (column 1) and values (column 3)
    ids <- data[[1]]
    values <- data[[3]]
    # Extract the subdirectory name
    folder_name <- basename(sub_dir)
    # If final_df is still empty, initialize it with the IDs
    if (nrow(final_df) == 0) {
      final_df <- data.frame(ID = ids, stringsAsFactors = FALSE)
    }
    # Create a temporary DataFrame with the extracted values
    temp_df <- data.frame(ID = ids, Value = values, stringsAsFactors = FALSE)
    names(temp_df)[2] <- folder_name  # Set column name as the folder name
    # Merge the new data with the final DataFrame, keeping all IDs
    final_df <- merge(final_df, temp_df, by = "ID", all = TRUE)
  }
}
#print(final_df)

## Correlations between perplexity of models
# Remove the ID column for correlation calculations
numeric_data <- final_df[, -1]
numeric_data <- apply(numeric_data, 2, as.numeric)
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
print(cor_matrix)
# Convert correlation matrix to a long-format data frame
cor_df <- as.data.frame(as.table(cor_matrix))
# Remove self-correlations (diagonal values where Var1 == Var2)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]
# Sort by correlation value (descending), then alphabetically by variable names
cor_df <- cor_df[order(-abs(cor_df$Freq)), ]
# Remove duplicate pairs (since correlation is symmetric, e.g., A-B and B-A)
cor_df <- cor_df[!duplicated(apply(cor_df[, 1:2], 1, function(x) paste(sort(x), collapse = "_"))), ]
# Select & print the top 6 correlations
top_6 <- head(cor_df, 6)
print(top_6)

## Correlation between ratings of models
condensedData <- readRDS("Data/condensedData.rds")
rating_columns <- grep("_Rating$", names(condensedData), value = TRUE)
rating_data <- condensedData[, rating_columns, drop = FALSE]
rating_data <- apply(rating_data, 2, as.numeric)
rating_cor_matrix <- cor(rating_data, use = "pairwise.complete.obs", method = "pearson")
print(rating_cor_matrix)
rating_cor_df <- as.data.frame(as.table(rating_cor_matrix))
# Remove self-correlations (diagonal values where Var1 == Var2)
rating_cor_df <- rating_cor_df[rating_cor_df$Var1 != rating_cor_df$Var2, ]
# Sort by correlation value (descending), then alphabetically by variable names
rating_cor_df <- rating_cor_df[order(-abs(rating_cor_df$Freq)), ]
# Remove duplicate pairs (since correlation is symmetric, e.g., A-B and B-A)
rating_cor_df <- rating_cor_df[!duplicated(apply(rating_cor_df[, 1:2], 1, function(x) paste(sort(x), collapse = "_"))), ]
# Select & print the top 6 correlations
rat_top_6 <- head(rating_cor_df, 6)
print(rat_top_6)

## Correlation between perplexities and ratings of models
# Merge final_df and condensedData by "ID", keeping only IDs from final_df
selected_columns <- c("ID", rating_columns)  # Include ID for merging
condensed_ratings <- condensedData[, selected_columns]
# Merge final_df with the filtered condensedData based on ID, keeping only IDs from final_df
merged_df <- merge(final_df, condensed_ratings, by = "ID", all.x = TRUE)
# Identify all columns that end with _ppx
ppx_columns <- grep("_ppx$", names(merged_df), value = TRUE)
# Create a data frame to store correlation results
cor_results <- data.frame(Prefix = character(), ModPPXModRtgCor = numeric(), stringsAsFactors = FALSE)
# Loop through all _ppx columns
for (ppx_col in ppx_columns) {
  # Extract the prefix (everything before "_ppx")
  prefix <- sub("_ppx$", "", ppx_col)
  # Find corresponding _Rating columns that match the prefix
  rating_candidates <- grep(paste0("^", prefix, ".*_Rating$"), names(merged_df), value = TRUE)
  # Loop through all possible rating columns for this prefix
  for (rating_col in rating_candidates) {
    x <- as.numeric(merged_df[[ppx_col]])
    y <- as.numeric(merged_df[[rating_col]])
    cor_value <- cor(x, y, use = "pairwise.complete.obs", method = "pearson")
    cor_results <- rbind(cor_results, data.frame(Prefix = prefix, ModPPXModRtgCor = cor_value, stringsAsFactors = FALSE))
  }
}
# Sort results by absolute correlation value (descending)
cor_results <- cor_results[order(-abs(cor_results$ModPPXModRtgCor)), ]
# Print the correlation results
print(cor_results)

#Correlation between human ratings and model perplexities
# Convert "Human_Rating" to numeric
human_rating <- as.numeric(merged_df$Human_Rating)
# Create a data frame to store the correlation results
human_cor_results <- data.frame(Prefix = character(), ModPPXHumRtgCor = numeric(), stringsAsFactors = FALSE)
# Loop through all _ppx columns
for (ppx_col in ppx_columns) {
  # Extract the prefix (everything before "_ppx")
  prefix <- sub("_ppx$", "", ppx_col)
  # Convert ppx column to numeric
  ppx_values <- as.numeric(merged_df[[ppx_col]])
  # Compute Pearson correlation, handling missing values
  cor_value <- cor(human_rating, ppx_values, use = "pairwise.complete.obs", method = "pearson")
  human_cor_results <- rbind(human_cor_results, data.frame(Prefix = prefix, ModPPXHumRtgCor = cor_value, stringsAsFactors = FALSE))
}
# Sort results by absolute correlation value (descending)
human_cor_results <- human_cor_results[order(-abs(human_cor_results$ModPPXHumRtgCor)), ]
print(human_cor_results)


# Merge results: ModPPXHumRtgCor - ModPPXModRtgCor -ModRtgHumRtgCor  
# Extract rows where Var1 is "Human_Rating"
human_rating_cor_df <- rating_cor_df[rating_cor_df$Var2 == "Human_Rating", -which(names(rating_cor_df) == "Var2")]
# Shorten Var2 by removing everything after the second underscore
human_rating_cor_df$Var1 <- sub("^([^_]+_[^_]+).*", "\\1", human_rating_cor_df$Var1)
names(human_rating_cor_df)[names(human_rating_cor_df) == "Var1"] <- "Prefix"
names(human_rating_cor_df)[names(human_rating_cor_df) == "Freq"] <- "ModRtgHumRtgCor"
print(human_rating_cor_df)
finalPPXResults <- merge(human_cor_results, cor_results, by = "Prefix", all.x=TRUE, sort = FALSE)
finalPPXResults <- merge(finalPPXResults, human_rating_cor_df, by = "Prefix", all.x = FALSE, sort = FALSE)
print(finalPPXResults)
write_tsv(finalPPXResults, "temp/finalppxresults.tsv")

