# From 03B1

# Mapping AntecedentE to roles
data <- grouped_scaled_average_ratings
data$Role <- recode(data$AntecedentE, "NP1" = "AGENT", "NP2" = "RECIPIENT", "NP3" = "PATIENT")



# Berechnung der Mittelwerte und Standardabweichungen
summary_data <- data %>%
  group_by("average_ratings$AntecedentE", "average_ratings$REP") %>%
  summarize(mean_rating = mean(Human_Rating, na.rm = TRUE), sd_rating = sd(Human_Rating, na.rm = TRUE)) %>%
  filter("average_ratings$REP" %in% c("Er", "Der", "Dieser", "Die", "Diese", "Sie"))

# Erstellen des Barplots
ggplot(summary_data, aes(x = "average_ratings$AntecedentE", y = mean_rating, fill = "average_ratings$REP")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_rating - sd_rating, ymax = mean_rating + sd_rating), 
                position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "Human Ratings by Antecedent and Pronoun", x = "Antecedent", y = "Mean zScore") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3", name = "Pronoun")

# grouped_scaled_ratings <- aggregate(. ~ scaled_average_ratings$REP + scaled_average_ratings$AntecedentE, data = scaled_average_ratings[rating_columns], FUN = mean, na.rm = TRUE)
#numeric_columns <- rating_columns[sapply(average_ratings[rating_columns], is.numeric)]

# Calculate mean relative to each REPType/AntecedentE tuple
grouped_average_ratings <- aggregate(average_ratings[, numeric_columns], 
                                     by = list(REP = average_ratings$REP, AntecedentE = average_ratings$AntecedentE), 
                                     FUN = mean, na.rm = TRUE)









# Initialize euclidian distance matrix
n <- length(rating_columns)
distance_matrix <- matrix(0, n, n)
rownames(distance_matrix) <- rating_columns
colnames(distance_matrix) <- rating_columns

for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    
    valid_rows <- complete.cases(average_ratings[[rating_columns[i]]], average_ratings[[rating_columns[j]]])
    sum_of_squares <- sum((average_ratings[valid_rows, rating_columns[i]] - average_ratings[valid_rows, rating_columns[j]])^2)
    num_valid_rows <- sum(valid_rows)
    print(num_valid_rows)
    print(rating_columns[i])
    print(rating_columns[j])
    print(" ")
    if (num_valid_rows > 0) {
      distance <- sqrt(sum_of_squares) / num_valid_rows
    } else {
      distance <- NA  
    }
    
    distance_matrix[i, j] <- distance
    distance_matrix[j, i] <- distance  
  }
}

distance_matrix




# Initialise cosine distance matrix
n <- length(rating_columns)
cosine_distance_matrix <- matrix(0, 1, n)
rownames(cosine_distance_matrix) <- rating_columns[1]
colnames(cosine_distance_matrix) <- rating_columns

# calculate cosine distance matrix
for (i in 1){#:(n-1)) {
  for (j in (i+1):n) {
    
    valid_rows <- complete.cases(average_ratings[[rating_columns[i]]], average_ratings[[rating_columns[j]]])
    if(i==1 & j==7){
      print(cosine_distance_matrix[i,j])
    }
    
    
    vec_a <- average_ratings[valid_rows, rating_columns[i]]
    vec_b <- average_ratings[valid_rows, rating_columns[j]]
    
    
    if (length(vec_a) > 0 && length(vec_b) > 0) {
      cosine_similarity <- sum(vec_a * vec_b) / (sqrt(sum(vec_a^2)) * sqrt(sum(vec_b^2)))
      cosine_distance <- 1 - cosine_similarity
    } else {
      cosine_distance <- NA  
    }
    
    cosine_distance_matrix[i, j] <- cosine_distance
    #cosine_distance_matrix[j, i] <- cosine_distance  # Matrix ist symmetrisch
  }
}

cosine_distance_matrix



# Extrahieren der ersten Zeile der Matrix
first_row <- cosine_distance_matrix[1, ]

# Erstellen eines Balkendiagramms aus der ersten Zeile
barplot(first_row, horiz = TRUE, las = 1, names.arg = colnames(cosine_distance_matrix), main ="Cosine Distance Ratings")


n <- length(rating_columns)

# Initialisieren der Matrix für die Kosinus-Distanzen
cosine_distance_matrix <- matrix(0, 1, n)
rownames(cosine_distance_matrix) <- rating_columns[1]
colnames(cosine_distance_matrix) <- rating_columns

# Initialisieren einer Liste, um die PromptIDs mit den größten Abweichungen zu speichern
max_deviation_ids <- list()

# Berechnung der Kosinus-Distanzen und Erfassung der größten Abweichungen
for (j in 2:n) {
  # Identifizieren der Zeilen ohne NA in beiden Spalten
  valid_rows <- complete.cases(average_ratings[[rating_columns[1]]], average_ratings[[rating_columns[j]]])
  
  # Vektoren für die Berechnung
  vec_a <- average_ratings[valid_rows, rating_columns[1]]
  vec_b <- average_ratings[valid_rows, rating_columns[j]]
  
  # Berechnen der Kosinus-Distanz
  if (length(vec_a) > 0 && length(vec_b) > 0) {
    cosine_similarity <- sum(vec_a * vec_b) / (sqrt(sum(vec_a^2)) * sqrt(sum(vec_b^2)))
    cosine_distance <- 1 - cosine_similarity
  } else {
    cosine_distance <- NA  # Setze NA, wenn keine gültigen Zeilen vorhanden sind
  }
  
  cosine_distance_matrix[1, j] <- cosine_distance
  
  # Erfassen der größten Abweichungen und der zugehörigen PromptIDs
  deviations <- abs(vec_a - vec_b)
  sorted_indices <- order(deviations, decreasing = TRUE)[1:5]
  max_deviation_ids[[rating_columns[j]]] <- average_ratings$PromptID[valid_rows][sorted_indices]
}

# Ausgabe der Kosinus-Distanzmatrix und der größten Abweichungs-IDs
list(CosineDistanceMatrix = cosine_distance_matrix, MaxDeviationIDs = max_deviation_ids)

n <- length(rating_columns)

# Initialisieren der Matrix für die Kosinus-Distanzen
cosine_distance_matrix <- matrix(0, 1, n)
rownames(cosine_distance_matrix) <- rating_columns[1]
colnames(cosine_distance_matrix) <- rating_columns

# Initialisieren einer Liste, um die Daten der größten Abweichungen zu speichern
max_deviation_data <- list()

# Berechnung der Kosinus-Distanzen und Erfassung der größten Abweichungen
for (j in 2:n) {
  # Identifizieren der Zeilen ohne NA in beiden Spalten
  valid_rows <- complete.cases(average_ratings[[rating_columns[1]]], average_ratings[[rating_columns[j]]])
  
  # Vektoren für die Berechnung
  vec_a <- average_ratings[valid_rows, rating_columns[1]]
  vec_b <- average_ratings[valid_rows, rating_columns[j]]
  
  # Berechnen der Kosinus-Distanz
  if (length(vec_a) > 0 && length(vec_b) > 0) {
    cosine_similarity <- sum(vec_a * vec_b) / (sqrt(sum(vec_a^2)) * sqrt(sum(vec_b^2)))
    cosine_distance <- 1 - cosine_similarity
  } else {
    cosine_distance <- NA  # Setze NA, wenn keine gültigen Zeilen vorhanden sind
  }
  
  cosine_distance_matrix[1, j] <- cosine_distance
  
  # Erfassen der größten Abweichungen und der zugehörigen Daten
  deviations <- abs(vec_a - vec_b)
  sorted_indices <- order(deviations, decreasing = TRUE)[1:5]
  deviation_ids <- average_ratings$PromptID[valid_rows][sorted_indices]
  deviation_ratings <- average_ratings[valid_rows, ][sorted_indices, rating_columns[j]]
  
  # Speichern der Daten in einem DataFrame
  max_deviation_data[[rating_columns[j]]] <- data.frame(
    PromptID = deviation_ids,
    Rating = deviation_ratings
  )
}

# Ausgabe der Kosinus-Distanzmatrix und der größten Abweichungsdaten
list(CosineDistanceMatrix = cosine_distance_matrix, MaxDeviationData = max_deviation_data)



## From here: Check which prompts show the biggest deviation. 

n <- length(rating_columns)

# Initialisieren der Matrix für die Kosinus-Distanzen
cosine_distance_matrix <- matrix(0, 1, n)
rownames(cosine_distance_matrix) <- rating_columns[1]
colnames(cosine_distance_matrix) <- rating_columns

# Initialisieren einer Liste, um die Daten der größten Abweichungen zu speichern
max_deviation_data <- list()

# Berechnung der Kosinus-Distanzen und Erfassung der größten Abweichungen
for (j in 2:n) {
  # Identifizieren der Zeilen ohne NA in beiden Spalten
  valid_rows <- complete.cases(average_ratings[[rating_columns[1]]], average_ratings[[rating_columns[j]]])
  
  # Vektoren für die Berechnung
  vec_a <- average_ratings[valid_rows, rating_columns[1]]
  vec_b <- average_ratings[valid_rows, rating_columns[j]]
  
  # Berechnen der Kosinus-Distanz
  if (length(vec_a) > 0 && length(vec_b) > 0) {
    cosine_similarity <- sum(vec_a * vec_b) / (sqrt(sum(vec_a^2)) * sqrt(sum(vec_b^2)))
    cosine_distance <- 1 - cosine_similarity
  } else {
    cosine_distance <- NA  # Setze NA, wenn keine gültigen Zeilen vorhanden sind
  }
  
  cosine_distance_matrix[1, j] <- cosine_distance
  
  # Erfassen der größten Abweichungen und der zugehörigen Daten
  deviations <- abs(vec_a - vec_b)
  sorted_indices <- order(deviations, decreasing = TRUE)[1:5]
  deviation_ids <- average_ratings$PromptID[valid_rows][sorted_indices]
  deviation_ratings_a <- average_ratings[valid_rows, ][sorted_indices, rating_columns[1]]
  deviation_ratings_b <- average_ratings[valid_rows, ][sorted_indices, rating_columns[j]]
  
  # Speichern der Daten in einem DataFrame
  max_deviation_data[[rating_columns[j]]] <- data.frame(
    PromptID = deviation_ids,
    RatingReference = deviation_ratings_a,
    RatingCompared = deviation_ratings_b
  )
}

# Ausgabe der Kosinus-Distanzmatrix und der größten Abweichungsdaten
list(CosineDistanceMatrix = cosine_distance_matrix, MaxDeviationData = max_deviation_data)


# From 03A AnalyseCompletion Answers

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

# TODO: Barplots 
