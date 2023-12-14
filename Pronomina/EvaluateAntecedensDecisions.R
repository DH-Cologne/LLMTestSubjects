# Run 01 and 02A before running this script
rm(list=ls())

library(readr)

# Definieren des Basisverzeichnisses
base_dir <- "LLMAnswers/Completions/"

# Initialisieren einer Liste, um die Zählungen zu speichern
counts <- list()

# Iteration über jeden Unterordner im Basisverzeichnis
subdirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
for (subdir in subdirs) {
  # Initialisieren der Zählungen für diesen Ordner
  count_zeros <- 0
  count_ones <- 0
  
  # Iteration über jede Datei im Unterordner
  files <- list.files(subdir, pattern = "\\.csv$", full.names = TRUE)
  files
  for (file in files) {
    # Versuchen, die Datei zu lesen, überspringen wenn weniger als 6 Spalten vorhanden sind
    tryCatch({
      data <- read_tsv(file, col_types = cols())
      summary(data)
      if (ncol(data) >= 6) {
        # Zählen von Nullen und Einsen in der 6. Spalte
        column6 <- data[[6]]
        count_zeros <- count_zeros + sum(column6 == 0, na.rm = TRUE)
        count_ones <- count_ones + sum(column6 == 1, na.rm = TRUE)
      }
    }, error = function(e) {})
  }
  
  # Speichern der Zählungen in der Liste
  counts[[basename(subdir)]] <- c(Zeros = count_zeros, Ones = count_ones)
}

# Ausgabe der Zählungen

library(readr)
library(dplyr)

# Definieren des Basisverzeichnisses
base_dir <- "LLMAnswers/Completions/"

# Initialisieren eines Vektors für die Ergebnisse
results <- matrix(nrow = 0, ncol = 3)
colnames(results) <- c("Directory", "Zeros", "Ones")

# Iteration über jeden Unterordner im Basisverzeichnis
subdirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
for (subdir in subdirs) {
  # Initialisieren der Zählungen für diesen Ordner
  count_zeros <- 0
  count_ones <- 0
  
  # Iteration über jede Datei im Unterordner
  files <- list.files(subdir, pattern = "\\.csv$", full.names = TRUE)
  for (file in files) {
    # Versuchen, die Datei zu lesen, überspringen wenn weniger als 6 Spalten vorhanden sind
    tryCatch({
      data <- read_tsv(file, col_types = cols())
      if (ncol(data) >= 6) {
        # Zählen von Nullen und Einsen in der 6. Spalte
        column6 <- data[[6]]
        count_zeros <- count_zeros + sum(column6 == 0, na.rm = TRUE)
        count_ones <- count_ones + sum(column6 == 1, na.rm = TRUE)
      }
    }, error = function(e) {})
  }
  
  # Hinzufügen der Zählungen zur Ergebnismatrix
  results <- rbind(results, c(basename(subdir), count_zeros, count_ones))
}

# Konvertieren der Ergebnismatrix in einen DataFrame
df_counts <- as.data.frame(results)
df_counts$Zeros <- as.integer(df_counts$Zeros)
df_counts$Ones <- as.integer(df_counts$Ones)

# Ausgabe des DataFrames
df_counts$Accuracy = df_counts$Ones/(df_counts$Zeros+df_counts$Ones)



