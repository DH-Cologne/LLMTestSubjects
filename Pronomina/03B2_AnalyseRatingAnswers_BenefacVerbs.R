rm(list=ls())

# Function to extract the last word of a string
extractLastWord <- function(s) {
  # Regular expression that removes everything except the last word
  lastWord <- sub(".*\\s(\\w+)[^\\w]*$", "\\1", s)
  return(lastWord)
}

# Read original Dataframe
expBAnswers <- readRDS("Data/ExpBDataAnswers.rds")
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

# Replace all values outside 0<x<8 by NA
replace_out_of_range <- function(column) {
  column[!(column %in% 1:7)] <- NA
  return(column)
}
condensedData[rating_columns] <-lapply(condensedData[rating_columns], replace_out_of_range)
condensedData[rating_columns] <- lapply(condensedData[rating_columns], as.numeric)

# Add PromptID to condensedData
condensedData$PromptID = paste(expBAnswers$ContextS, substr(expBAnswers$TargetPrompt, 1, nchar(expBAnswers$TargetPrompt)-51))


# Calculate mean for each PromptID for each model and build a DataFrame for average_ratings
average_ratings <- aggregate(condensedData[rating_columns], by = list(condensedData$PromptID), FUN = function(x) mean(x, na.rm = TRUE))
colnames(average_ratings)[1] <- "PromptID"
REP_data <- unique(condensedData[, c("PromptID", "REP")])
average_ratings <- merge(average_ratings, REP_data, by = "PromptID")
AntecedentE_data <- unique(condensedData[, c("PromptID", "AntecedentE")])
average_ratings <- merge(average_ratings, AntecedentE_data, by = "PromptID")
summary(average_ratings)


# Build REPType/AntecedentE tuples
grouped_average_ratings <- aggregate(. ~ REP + AntecedentE, data = average_ratings[2:13], FUN = mean, na.rm = TRUE)
numeric_columns <- rating_columns[sapply(average_ratings[rating_columns], is.numeric)]

# Calculate mean relative to each REPType/AntecedentE tuple
grouped_average_ratings <- aggregate(average_ratings[, numeric_columns], 
                                     by = list(REP = average_ratings$REP, AntecedentE = average_ratings$AntecedentE), 
                                     FUN = mean, na.rm = TRUE)

#grouped_average_ratings <- grouped_average_ratings[, -c(4:7)]

summary(grouped_average_ratings)

# zTransformation of average ratings
scaled_average_ratings <- average_ratings
scaled_average_ratings[rating_columns] <- scale(average_ratings[rating_columns])
summary(scaled_average_ratings)

# Plot average ratings
# Needs more space on the left side
old_par <- par()
par(mar = c(5, 12, 4, 2) + 0.1)

# boxplot average ratings
boxplot(average_ratings[rating_columns], horizontal = TRUE, yaxt ="n")
axis(2, at = 1:length(average_ratings[rating_columns]), labels = colnames(average_ratings[rating_columns]), las = 1)

# boxplot scaled average ratings
boxplot(scaled_average_ratings[rating_columns], horizontal = TRUE, yaxt ="n")
axis(2, at = 1:length(scaled_average_ratings[rating_columns]), labels = colnames(scaled_average_ratings[rating_columns]), las = 1)

par(old_par)







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

