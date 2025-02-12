# This code can be used to read out the answers given by the LLMs and aggregates 
# them together with the experiment data (ExpBPPXData).

# The study examined ditransitive verbs (ExpB1) and benefactive verbs (ExpB2). 
# Ditransitive verbs involve three arguments (agent, recipient, and patient), 
# such as "give" or "send," where a possession is transferred. 
# Benefactive verbs involve actions performed for the benefit of a third party, 
# like "buy" in "buy a gift for someone," involving an agent, benefactive 
# recipient, and patient.

rm(list=ls())

source("99_Utils.R")

## Load perplexity answers to DataFrame
base_dir <- "LLMAnswers/Perplexity"
sub_dirs <- list.dirs(base_dir, recursive = FALSE)
final_df <- data.frame(ID = character(), Value = numeric(), ExPID = character(), stringsAsFactors = FALSE)

for (sub_dir in sub_dirs) {
  print(sub_dir)
  tsv_files <- list.files(sub_dir, full.names = TRUE)
  modelName <- split_at_nth_underscore(basename(sub_dir),2)
  model_df <- data.frame(ID = character(), Value = numeric(), ExPID = character(), stringsAsFactors = FALSE)
  
  for (file in tsv_files) {
    expNumber <- split_at_nth_underscore(basename(file),1)
    
    data <- read.delim(file, header = FALSE, stringsAsFactors = FALSE)
    # Ensure the file has at least 2 columns before processing
    if (ncol(data) < 2) next
    
    # Extract columns: IDs (column 1) and values (column 2), set ExpID
    ids <- data[[1]]
    values <- data[[2]]
    expID <- expNumber
    
    # Extract the subdirectory name
    folder_name <- basename(sub_dir)
    
    temp_df <- data.frame(ID = ids, Value = values, ExpID = expNumber, Model = modelName, stringsAsFactors = FALSE)
    
    # Vertical binding
    model_df <- rbind(model_df, temp_df)
  }
  # Vertical binding
  final_df <- rbind(final_df, model_df)
}

# reshape results for export
final_df_wide <- reshape(final_df, 
                         idvar = c("ID", "ExpID"), 
                         timevar = "Model", 
                         direction = "wide")
colnames(final_df_wide) <- gsub("^Value\\.", "", colnames(final_df_wide))

#write.table(final_df_wide, file = "Data/model_perplexities.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
saveRDS(final_df_wide, file = "Data/model_perplexities.rds")

