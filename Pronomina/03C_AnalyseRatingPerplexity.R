rm(list=ls())
source("99_Utils.R")

## Load model perplexities to DataFrame
model_ppx <- readRDS(file = "Data/model_perplexities.rds")
# summary(model_ppx)
# str(model_ppx)

## Load and transform Rating answers
ratingsExp1 <- read.delim("Data/corrected_average_ratings_ExpB1.tsv", sep = "\t", stringsAsFactors = FALSE)
ratingsExp2 <- read.delim("Data/corrected_average_ratings_ExpB2.tsv", sep = "\t", stringsAsFactors = FALSE)
# Combine experiments
model_ratings <- rbind(ratingsExp1, ratingsExp2)
colnames(model_ratings) <- sapply(colnames(model_ratings), function(x) split_at_nth_underscore(x, 2))
# Reorder columns
model_ratings <- model_ratings[, c(1, (ncol(model_ratings)-1):ncol(model_ratings), 2:(ncol(model_ratings)-2))]
colnames(model_ratings)[colnames(model_ratings) == "PromptID"] <- "ID"
# str(model_ratings)

## Merge both data frames by "PromptID", keeping all rows (full outer join)
merged_df <- merge(model_ratings, model_ppx, by = "ID", all = TRUE, suffixes = c(".rat", ".ppx"))
# Define new column order: Keep "PromptID" first, then "ExpID", then the rest
new_order <- c("ID", "ExpID", setdiff(colnames(merged_df), c("ID", "ExpID")))
merged_df <- merged_df[, new_order]
# str(merged_df)

## Correlations between perplexity of models
# Remove the ID column for correlation calculations
ppx_data <- model_ppx[, -c(1,2)]
#ppx_data <- apply(ppx_data, 2, as.numeric)
cor_matrix <- cor(ppx_data, use = "pairwise.complete.obs", method = "spearman")
# print(cor_matrix)
# Convert correlation matrix to a long-format data frame
ppx_cor_df <- as.data.frame(as.table(cor_matrix))
# Remove self-correlations (diagonal values where Var1 == Var2)
ppx_cor_df <- ppx_cor_df[ppx_cor_df$Var1 != ppx_cor_df$Var2, ]
# Sort by correlation value (descending), then alphabetically by variable names
ppx_cor_df <- ppx_cor_df[order(-abs(ppx_cor_df$Freq)), ]
# Remove duplicate pairs (since correlation is symmetric, e.g., A-B and B-A)
ppx_cor_df <- ppx_cor_df[!duplicated(apply(ppx_cor_df[, 1:2], 1, function(x) paste(sort(x), collapse = "_"))), ]
print("Correlation between perplexities of models")
print(ppx_cor_df)
# Define a color palette
col_palette <- colorRampPalette(c("white", "yellow", "orange"))(100)
# Plot heatmap
heatmap(cor_matrix, col=col_palette, scale = "none", 
        symm = TRUE, margins = c(7,7), cexRow = 0.7, cexCol = 0.7)


## Correlation between ratings of models
rating_data <- model_ratings[, -c(1:3), drop = FALSE]
rating_data <- apply(rating_data, 2, as.numeric)
rating_cor_matrix <- cor(rating_data, use = "pairwise.complete.obs", method = "spearman")
rating_cor_df <- as.data.frame(as.table(rating_cor_matrix))
# Remove self-correlations (diagonal values where Var1 == Var2)
rating_cor_df <- rating_cor_df[rating_cor_df$Var1 != rating_cor_df$Var2, ]
# Sort by correlation value (descending), then alphabetically by variable names
rating_cor_df <- rating_cor_df[order(-abs(rating_cor_df$Freq)), ]
# Remove duplicate pairs (since correlation is symmetric, e.g., A-B and B-A)
rating_cor_df <- rating_cor_df[!duplicated(apply(rating_cor_df[, 1:2], 1, function(x) paste(sort(x), collapse = "_"))), ]
print("Correlations between ratings of models")
print(rating_cor_df)
# Plot heatmap
heatmap(rating_cor_matrix, col=col_palette, scale = "none", 
        symm = TRUE, margins = c(7,7), cexRow = 0.7, cexCol = 0.7)

## Correlation between perplexities and ratings of models
# Identify all .ppx and .rat columns
ppx_cols <- grep("\\.ppx$", colnames(merged_df), value = TRUE)
rat_cols <- gsub("\\.ppx$", ".rat", ppx_cols)  # Generate corresponding .rat column names
# Ensure only existing .rat columns are included
rat_cols <- rat_cols[rat_cols %in% colnames(merged_df)]
ppx_cols <- ppx_cols[ppx_cols %in% colnames(merged_df)]  # Ensure matching pairs
# Compute correlations for matching .ppx and .rat columns
cor_results_PR <- sapply(seq_along(ppx_cols), function(i) {
  cor(merged_df[[ppx_cols[i]]], merged_df[[rat_cols[i]]], use = "pairwise.complete.obs", method = "spearman")
})
# Convert results into a DataFrame
cor_results_PRdf <- data.frame(Model = ppx_cols, MP_MR = cor_results_PR)
cor_results_PRdf <- cor_results_PRdf[order(cor_results_PRdf$MP_MR, decreasing = FALSE), ]
cor_results_PRdf$Model <- gsub("\\.ppx$", "", cor_results_PRdf$Model)
print(cor_results_PRdf)


##Correlation between human ratings and model perplexities
# Identify all .ppx columns
# Compute correlations between each .ppx column and Human_Rating
cor_results_PHR <- sapply(ppx_cols, function(col) {
  cor(merged_df[[col]], merged_df[["Human_Rating"]], use = "pairwise.complete.obs", method = "spearman")
})
# Convert results into a DataFrame
cor_results_PHRdf <- data.frame(Model = ppx_cols, MP_HR = cor_results_PHR)
# Sort by absolute correlation value (highest to lowest)
cor_results_PHRdf <- cor_results_PHRdf[order(cor_results_PHRdf$MP_HR, decreasing = FALSE), ]
cor_results_PHRdf$Model <- gsub("\\.ppx$", "", cor_results_PHRdf$Model)
# Print results
print(cor_results_PHRdf)


##Correlation between human ratings and model ratings
# Identify all .ppx columns
# Compute correlations between each .ppx column and Human_Rating
cor_results_RHR <- sapply(rat_cols, function(col) {
  cor(merged_df[[col]], merged_df[["Human_Rating"]], use = "pairwise.complete.obs", method = "spearman")
})
# Convert results into a DataFrame
cor_results_RHRdf <- data.frame(Model = rat_cols, MR_HR = cor_results_RHR)
# Sort by absolute correlation value (highest to lowest)
cor_results_RHRdf <- cor_results_RHRdf[order(cor_results_RHRdf$MR_HR, decreasing = FALSE), ]
cor_results_RHRdf$Model <- gsub("\\.rat$", "", cor_results_RHRdf$Model)
# Print results
print(cor_results_RHRdf)

## Build table for all correlations 
# Merge the first two DataFrames
merged_results <- merge(cor_results_RHRdf, cor_results_PHRdf, by = "Model", all = TRUE)
# Merge the third DataFrame
merged_results <- merge(merged_results, cor_results_PRdf, by = "Model", all = TRUE)
column_order <- c("Model", "MP_MR", "MP_HR", "MR_HR")
merged_results <- merged_results[, column_order]
merged_results <- merged_results[order(merged_results$MR_HR, decreasing = FALSE), ]

# Print the merged DataFrame
print(merged_results)

library(xtable)

# Function to round values and bold the maximum per column
bold_max_rounded <- function(column) {
  max_val <- column[which.max(abs(column))]
  rounded_vals <- round(column, 2)  # Round values to 2 decimal places
  formatted <- ifelse(column == max_val, paste0("\\textbf{", rounded_vals, "}"), rounded_vals)  # Bold max value
  return(formatted)
}

# Apply the formatting to numeric columns (excluding the Model column)
merged_results_bold <- as.data.frame(lapply(merged_results[-1], bold_max_rounded))  # Exclude Model column
merged_results_bold <- cbind(Model = merged_results$Model, merged_results_bold)  # Reattach Model column

# Replace underscores with dashes in the Model column
merged_results_bold$Model <- gsub("_", "-", merged_results_bold$Model)

# Convert to LaTeX table with small font
latex_table <- xtable(merged_results_bold, caption = "Correlation Results with Bold Max Values", label = "tab:correlations")

# Print LaTeX table with proper formatting
print(latex_table, type = "latex", include.rownames = FALSE, sanitize.text.function = identity,
      floating = FALSE, tabular.environment = "tabular", add.to.row = list(
        pos = list(0),
        command = "\\small\n"  # Set smaller font size in LaTeX
      ))



## Merge genders, extract ExpB1, groupBy REP & Antecedent 
# Filter only rows where ExpID == "ExpB1"
grouped_means <- merged_df %>%
  filter(ExpID == "ExpB1") %>%
  mutate(REP_Grouped = case_when(
    REP %in% c("Der", "Die") ~ "Der/Die",
    REP %in% c("Dieser", "Diese") ~ "Dieser/Diese",
    REP %in% c("Er", "Sie") ~ "Er/Sie",
    TRUE ~ REP  # Keep other values unchanged
  )) %>%
  group_by(REP_Grouped, AntecedentE) %>%
  summarise(across(-c(1:3), mean, na.rm = TRUE), .groups = "drop")
# Print the final grouped means
print(grouped_means)

## Add publication data, manage merged dataset
# Read the publication data
pub_ratings_df <- read.delim("Data/Publication_Rating.tsv", sep = "\t", stringsAsFactors = FALSE)
# Merge the new dataset with df
merged_final <- grouped_means %>%
  left_join(pub_ratings_df, by = c("REP_Grouped", "AntecedentE"))
# Define the first three fixed columns
fixed_columns <- c("REP_Grouped", "AntecedentE", "Publication_Rating")
# Identify columns ending with .rat
rat_columns <- grep("\\.rat$", colnames(merged_final), value = TRUE)
# Identify columns ending with .ppx
ppx_columns <- grep("\\.ppx$", colnames(merged_final), value = TRUE)
# Identify remaining columns that do not fit the above categories
other_columns <- setdiff(colnames(merged_final), c(fixed_columns, rat_columns, ppx_columns))
# Combine all in the required order
new_column_order <- c(fixed_columns, rat_columns, ppx_columns, other_columns)
# Reorder the DataFrame
merged_final <- merged_final[, new_column_order]
merged_final[] <- lapply(merged_final, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})
merged_final <- merged_final[order(merged_final$AntecedentE, merged_final$REP_Grouped), ]

## Rating table to LATEX 
merged_latex <- merged_final[, 1:11]
# Replace underscores with dashes in column names
colnames(merged_latex) <- gsub("_", "-", colnames(merged_latex))
# Convert to LaTeX table
# Convert column names to rotated format
colnames_rotated <- paste0("\\rotatebox{90}{", colnames(merged_latex), "}")
# Convert to LaTeX table
latex_table <- xtable(merged_latex, caption = "First 11 Columns of Merged Data", label = "tab:merged_data")
# Print the table with rotated headers
print(latex_table, type = "latex", include.rownames = FALSE, sanitize.text.function = identity,
      add.to.row = list(pos = list(0), command = paste0("\\hline\n", paste(colnames_rotated, collapse = " & "), " \\\\\n\\hline\n")))



## Correlations with publication data
# Select only numeric columns (excluding first two)
numeric_data <- merged_final[, -c(1,2)]
# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "spearman")
# Extract correlations with "Publication_Rating" and sort by absolute value
pub_correlations <- cor_matrix["Publication_Rating", ]
pub_correlations_sorted <- pub_correlations[order(abs(pub_correlations), decreasing = TRUE)]
# Convert to DataFrame for better readability
pub_correlations_df <- data.frame(Variable = names(pub_correlations_sorted), Correlation = pub_correlations_sorted)
# Print the sorted correlations
print(pub_correlations_df)


# Load required library
library(ggplot2)

# Select numeric columns (excluding the first two: REP_Grouped, AntecedentE)
numeric_cols <- colnames(merged_final)[-c(1,2)]

# Create regression plots for each numeric column against "Publication_Rating"
for (col in numeric_cols) {
  p <- ggplot(merged_final, aes_string(x = col, y = "Publication_Rating")) +
    geom_point(alpha = 0.5) +          # Scatter plot with transparency
    geom_smooth(method = "lm", color = "red") +  # Linear regression line
    labs(title = paste("Regression Plot:", col, "vs Publication_Rating"),
         x = col, y = "Publication_Rating") +
    theme_minimal()
  print(p)  # Display each plot
}


##Correlation between human ratings and model perplexities
# Identify all .ppx columns
ppx_cols <- grep("\\.ppx$", colnames(grouped_means), value = TRUE)
# Compute correlations between each .ppx column and Human_Rating
grouped_cor_results <- sapply(ppx_cols, function(col) {
  cor(grouped_means[[col]], grouped_means[["Human_Rating"]], use = "pairwise.complete.obs", method = "spearman")
})
# Convert results into a DataFrame
grouped_cor_results_df <- data.frame(Model = ppx_cols, Correlation = grouped_cor_results)
# Sort by absolute correlation value (highest to lowest)
grouped_cor_results_df <- grouped_cor_results_df[order(grouped_cor_results_df$Correlation, decreasing = FALSE), ]
# Print results
print(grouped_cor_results_df)





### OLD STUFF:

colnames(model_ppx)[colnames(model_ppx) == "ID"] <- "PromptID"
model_ppx <- merge(model_ppx, model_ratings[, c("PromptID", "Human_Rating")], by = "PromptID", all.x = TRUE)
# Select only the numeric columns (excluding the first two columns)
numeric_cols <- colnames(model_ppx)[-c(1,2)]  # Excludes first two columns

# Compute correlation between "Human_Rating" and all other selected columns
cor_results <- sapply(numeric_cols, function(col) {
  cor(model_ppx[["Human_Rating"]], model_ppx[[col]], use = "pairwise.complete.obs", method = "pearson")
})

# Convert results to a DataFrame with column names
cor_results_df <- data.frame(Variable = numeric_cols, Correlation = cor_results)

# Print results
print(cor_results_df)


# Merge model_ppx and condensedData by "ID", keeping only IDs from model_ppx
selected_columns <- c("ID", rating_columns)  # Include ID for merging
condensed_ratings <- condensedData[, selected_columns]
# Merge model_ppx with the filtered condensedData based on ID, keeping only IDs from model_ppx
merged_df <- merge(model_ppx, condensed_ratings, by = "ID", all.x = TRUE)
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



common_cols <- intersect(colnames(model_ratings), colnames(model_ppx))
cor_results <- sapply(common_cols, function(col) {
  cor(model_ratings[[col]], model_ppx[[col]], use = "pairwise.complete.obs")
})
cor_results_df <- data.frame(Model = common_cols, Correlation = cor_results)
print(cor_results_df)

## Regression plots for ppx vs. rating - not so pretty...
# for (i in seq_along(ppx_cols)) {
#   p <- ggplot(merged_df, aes_string(x = ppx_cols[i], y = rat_cols[i])) +
#     geom_point(alpha = 0.6) +  # Scatter plot with transparency
#     geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
#     labs(title = paste("Regression Plot:", ppx_cols[i], "vs", rat_cols[i]),
#          x = ppx_cols[i], y = rat_cols[i]) +
#     theme_minimal() +
#     theme(text = element_text(size = 12))  # Adjust text size
#   
#   print(p)  # Display each plot
# }
