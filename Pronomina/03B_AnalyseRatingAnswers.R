rm(list=ls())
source("99_Utils.R")

# Read original Dataframe (choose between B1 and B2 data)
expBAnswers <- readRDS("Data/ExpB1DataAnswers.rds") #File with data for ditransitive verbs
# expBAnswers <- readRDS("Data/ExpB2DataAnswers.rds") #File with data for benefatorial verbs


columns <- grep("_Rating$", names(expBAnswers), value = TRUE)

# Build analysis data
condensedData <- data.frame(ID = expBAnswers$ID, 
                            ExperimentID = expBAnswers$ExperimentID,
                            AntecedentE = expBAnswers$AntecedentE,
                            REP = expBAnswers$REP,
                            Human_Rating = expBAnswers$Rating7,
                            expBAnswers[, columns])

# grep all columns that end with "Rating"
rating_columns <- grep("_Rating$", names(condensedData), value = TRUE)
condensedData[rating_columns] <-lapply(condensedData[rating_columns], replace_out_of_range)
condensedData[rating_columns] <- lapply(condensedData[rating_columns], as.numeric)
# Add PromptID to condensedData
condensedData$PromptID = paste(expBAnswers$ContextS, substr(expBAnswers$TargetPrompt, 1, nchar(expBAnswers$TargetPrompt)-51))

# count NAs of each model, visualize
na_counts <- sapply(condensedData[rating_columns], function(x) sum(is.na(x)))
na_table <- data.frame(Column = names(na_counts), NA_Count = na_counts)
ggplot(na_table, aes(x = Column, y = NA_Count)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  labs(
    title = "Number of NA Values (log scale)",
    x = "Model",
    y = "Number of NA Values"
  ) +
  theme_minimal() +
  coord_flip() +
  scale_y_log10() 
  

# Visualize rating distribution of models
long_data <- pivot_longer(condensedData, cols = all_of(rating_columns), names_to = "Model", values_to = "Rating_Value")
ggplot(long_data, aes(x = Model, y = Rating_Value)) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of ratings",
    x = "Model",
    y = "Rating Value"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(1, 7, by = 1))  # Ensure all y-axis labels (original x-axis) are displayed

# Calculate mean for each PromptID for each model and build a DataFrame for average_ratings
average_ratings <- aggregate(condensedData[rating_columns], by = list(condensedData$PromptID), FUN = function(x) mean(x, na.rm = TRUE))
colnames(average_ratings)[1] <- "PromptID"
REP_data <- unique(condensedData[, c("PromptID", "REP")])
average_ratings <- merge(average_ratings, REP_data, by = "PromptID")
AntecedentE_data <- unique(condensedData[, c("PromptID", "AntecedentE")])
average_ratings <- merge(average_ratings, AntecedentE_data, by = "PromptID")
summary(average_ratings)

# count NAs of each model, visualize
na_counts <- sapply(average_ratings[rating_columns], function(x) sum(is.na(x)))
na_table <- data.frame(Column = names(na_counts), NA_Count = na_counts)
ggplot(na_table, aes(x = Column, y = NA_Count)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  labs(
    title = "Number of NA Values (average ratings)",
    x = "Model",
    y = "Number of NA Values"
  ) +
  theme_minimal() +
  coord_flip()


# Visualize rating distribution of models
long_data <- pivot_longer(average_ratings, cols = all_of(rating_columns), names_to = "Model", values_to = "Rating_Value")
ggplot(long_data, aes(x = Model, y = Rating_Value)) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of average ratings",
    x = "Model",
    y = "Rating Value"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(1, 7, by = 1))  # Ensure all y-axis labels (original x-axis) are displayed

# zTransformation of average ratings
scaled_average_ratings <- average_ratings
scaled_average_ratings[rating_columns] <- scale(average_ratings[rating_columns])
summary(scaled_average_ratings)

# Visualize scaled rating distribution of models
long_data <- pivot_longer(scaled_average_ratings, cols = all_of(rating_columns), names_to = "Model", values_to = "Rating_Value")
ggplot(long_data, aes(x = Model, y = Rating_Value)) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of average ratings (scaled)",
    x = "Model",
    y = "Z Value of Ratings"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# Build REPType/AntecedentE tuples
grouped_average_ratings <- aggregate(. ~ average_ratings$REP + average_ratings$AntecedentE, data = average_ratings[rating_columns], FUN = mean, na.rm = TRUE)
summary(grouped_average_ratings)
#numeric_columns <- rating_columns[sapply(average_ratings[rating_columns], is.numeric)]

# Scale tuples
grouped_scaled_average_ratings <- grouped_average_ratings
grouped_scaled_average_ratings[rating_columns] <- scale(grouped_average_ratings[rating_columns])
summary(grouped_scaled_average_ratings)

## TODO: Visualize grouped_scaled_average_ratings 


