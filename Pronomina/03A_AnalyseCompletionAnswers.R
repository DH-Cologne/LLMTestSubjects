# Analyses of the completion answers from LLMs on sentences with ditransitive verbs (A1) and state verbs (A2)
rm(list=ls())
source("99_Utils.R")

# Datasets to process (A1: action verbs, A2: state verbs)
datasets <- list(
  "ExpA1" = "Data/ExpA1DataAnswers.rds",  # data for action verbs
  "ExpA2" = "Data/ExpA2DataAnswers.rds"  # data for state verbs
)


# Process each dataset
for (dataset_name in names(datasets)) {
  cat("\nProcessing dataset:", dataset_name, "\n")

  # Load the data
  expAAnswers <- readRDS(datasets[[dataset_name]])

  columns <- grep("Antecedens$", names(expAAnswers), value = TRUE)
  
  # Build analysis data
  condensedData <- data.frame(ID = expAAnswers$ID, 
                              RE1 = extractLastWord(expAAnswers$RE1), 
                              RE2 = extractLastWord(expAAnswers$RE2),
                              expAAnswers[, columns])
  
  cols_to_modify <- setdiff(names(condensedData), "ID")
  
  # condensedData<-  replace_redundantanswers(condensedData)
  
  for (col in cols_to_modify) {
    condensedData[[col]] <- shorten_words(condensedData[[col]])
  }
  
  # grep all columns that end with "Antecedens"
  antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)
  
  # replace Antecedences
  condensedData <- as.data.frame(t(apply(condensedData, 1, replace_values)))
  
  # Antecedences to factors
  condensedData[antecedens_columns] <- lapply(condensedData[antecedens_columns], factor)
  condensedData$Human_Antecedens <- expAAnswers$AntecedentAnswer
  # Replace: NP1 -> RE1, NP2 -> RE2, other -> NA
  condensedData$Human_Antecedens <- ifelse(condensedData$Human_Antecedens == "NP1", "RE1",
                                              ifelse(condensedData$Human_Antecedens == "NP2", "RE2", NA))
  condensedData$Human_Antecedens <- as.factor(condensedData$Human_Anteceden)
  antecedens_columns <- grep("Antecedens$", names(condensedData), value = TRUE)
  
  
  condensedData$AO = expAAnswers$ArgumentOrder
  condensedData$REP = as.factor(expAAnswers$REP)
  condensedData$ItemNumber = expAAnswers$ItemNumber
  summary(condensedData)
  # names(condensedData)
  
  
  
  # fuction to count RE1 and RE2 as Antecedences
  count_frequencies <- function(x) {
    c(RE1 = sum(x == "RE1", na.rm = TRUE),
      RE2 = sum(x == "RE2", na.rm = TRUE),
      NA_count = sum(is.na(x)))
  }
  
  # Aggregate data by ArgumentOrder (OS vs. SO) and Replacement-String (der vs. dieser vs. er)
  aggregated_data <- aggregate(condensedData[, antecedens_columns], 
                               by = list(AO = condensedData$AO, REP = condensedData$REP), #ItemNumber = condensedData$ItemNumber), 
                               FUN = count_frequencies)
  result <- do.call(data.frame, aggregated_data)
  
  
  # Calculate differences between counts of RE1 and RE2
  calculate_difference <- function(x) {
    sum(x == "RE1", na.rm = TRUE) - sum(x == "RE2", na.rm = TRUE)
  }
  
  # Calculate relative differences between counts of RE1 and RE2
  calculate_relative_difference <- function(x) {
    re1_count <- sum(x == "RE1", na.rm = TRUE)
    re2_count <- sum(x == "RE2", na.rm = TRUE)
    if (re1_count + re2_count > 0) {
      (re1_count - re2_count) / (re1_count + re2_count)
    } else {
      NA  # if no RE1 and RE2 are found
    }
  }
  
  # Aggregate data by ArgumentOrder (OS vs. SO) and Replacement-String (der vs. dieser vs. er)
  preferences_data <- aggregate(condensedData[, antecedens_columns], 
                               by = list(AO = condensedData$AO, REP = condensedData$REP), 
                               FUN = calculate_difference)
  result <- do.call(data.frame, preferences_data)
  
  # plot preferences
  par(mfrow = c(3, 4))
  plot_columns <- names(result)[4:length(result)]
  
  
  for (col in plot_columns) {
    bar_colors <- ifelse(result$AO == "OS", "lightblue", "darkblue")
    barplot(result[[col]], main = col, xlab = "Groups", ylab = "Rel.Diff", col = bar_colors)
    #legend("bottomright", legend = c("OS", "SO"), fill = c("lightblue", "darkblue"), bty = "n")
  }
  
  # Export data
  write.table(aggregated_data, file = "temp/subset_data_ExpA2.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Reimport data
  data <- read.table("temp/subset_data_ExpA2.tsv", sep = "\t", header = TRUE)
  
  ## NA-counts
  # Extract NA_count columns
  na_columns <- grep("NA_count$", names(data), value = TRUE)
  
  # NA_counts for each model
  na_results <- data.frame(
    Model = sub("\\.NA_count$", "", na_columns),  # Modellnamen extrahieren
    NA_Count = sapply(na_columns, function(col) sum(data[[col]], na.rm = TRUE))  # Summiere die Werte der NA_count-Spalten
  )
  #sort
  na_results <- na_results[order(-na_results$NA_Count), ]
  
  # export NA_counts
  write.table(na_results, file = paste0("temp/", dataset_name, "_NA_Count_Sorted.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
  print(na_results)
  
  ## ChiSquare
  
  # Extract: RE1 / RE2 columns from data
  model_columns <- grep("RE1$|RE2$", names(data), value = TRUE)  # Spalten mit RE1 und RE2 suchen
  
  # Extract model names from data
  models <- unique(sub("\\.RE1$|\\.RE2$", "", model_columns))   
  
  results <- list()
  
  # ChiSquare for all models
  for (model in models) {
    cat("Examination of model:", model, "\n")
    re1_col <- paste0(model, ".RE1")
    re2_col <- paste0(model, ".RE2")
    
    for (pronoun in unique(data$REP)) {
      cat("  Pronoun:", pronoun, "\n")
      subset_data <- data[data$REP == pronoun, ]
      
      contingency_table <- matrix(
        c(
          sum(subset_data[subset_data$AO == "OS", re1_col], na.rm = TRUE),
          sum(subset_data[subset_data$AO == "OS", re2_col], na.rm = TRUE),
          sum(subset_data[subset_data$AO == "SO", re1_col], na.rm = TRUE),
          sum(subset_data[subset_data$AO == "SO", re2_col], na.rm = TRUE)
        ),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c("OS", "SO"), c("RE1", "RE2"))
      )
      chi_result <- chisq.test(contingency_table)
      
      # save results
      results[[paste0(model, "_", pronoun)]] <- list(
        model = model,
        pronoun = pronoun,
        contingency_table = contingency_table,
        p_value = chi_result$p.value,
        chi_sq = chi_result$statistic
      )
      
      # print results
      print(contingency_table)
      cat("    Chi-Square:", chi_result$statistic, "\n")
      cat("    p-Wert:", chi_result$p.value, "\n")
    }
  }
  
  # export results
  results_df <- do.call(rbind, lapply(names(results), function(x) {
    cbind(Model = results[[x]]$model,
          Pronoun = results[[x]]$pronoun,
          Chi_Square = results[[x]]$chi_sq,
          P_Value = results[[x]]$p_value)
  }))
  results_df <- as.data.frame(results_df)
  write.table(results_df, paste0("temp/", dataset_name, "_ChiSquare_Results_per_Pronoun.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
  write.table(preferences_data, paste0("temp/", dataset_name, "_AntecedensPreferences.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
  
  
  # Spaltennamen extrahieren
  columns <- colnames(preferences_data)
  
  # Letzte Spalte als erwartete Werte
  expected <- preferences_data[[columns[length(columns)]]]
  
  # Ergebnisliste für Chi-Square-Werte
  chi_square_results <- list()
  
  # Für jede Spalte (außer der letzten) den Chi-Square-Wert berechnen
  for (col in columns[3:(length(columns) - 1)]) {
    observed <- preferences_data[[col]]
    
    # Chi-Square-Test durchführen
    chi_square <- sum((observed - expected)^2 / expected, na.rm = TRUE)
    
    # Ergebnis speichern
    chi_square_results[[col]] <- chi_square
  }
  
  chi_square_results <- chi_square_results[order(unlist(chi_square_results))]
  chi2_df <- data.frame(Value = unlist(chi_square_results), stringsAsFactors = FALSE)
  
  # Ergebnisse ausgeben
  print(chi2_df)
  
  
  
  # Optional: Ergebnisse in einer Datei speichern
  write.table(chi2_df, paste0("temp/", dataset_name,"_ChiSquareResults_allInOne.tsv"), sep = "\t", quote = FALSE, col.names = NA)
  
}
    
