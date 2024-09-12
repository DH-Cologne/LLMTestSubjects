# This code can be used to read out the answers given by the LLMs and aggregates 
# them together with the experiment data (ExpAData) within two a new data frames 
# (ExpA1DataAnswers and ExpA2DataAnswers).
#
# In ExpA1, the study focuses on action verbs, which describe dynamic activities 
# and often imply a direct relationship between the subject and the action performed.
# In ExpA2, state verbs are examined, which describe more static conditions or 
# states of being and do not typically involve dynamic action.


rm(list=ls())
source("99_Utils.R")

# Function to check conditions and return according to your rules
check_and_return <- function(string1, string2, string3) {
  word1 <- extractLastWord(string1)
  word2 <- extractLastWord(string2)
  words3 <- unlist(strsplit(string3, "\\s+"))
  if (word1 %in% words3) {
    return(word1)
  } else if (word2 %in% words3) {
    return(word2)
  } else {
    notfound <- notfound+1
    return(extractLastWord(string3))
  }
}

# Read original Dataframe
expAData <- readRDS("Data/ExpAData.rds")

answerfolders = "LLMAnswers/Completions/"

# List files in destination folder 
folderList <- list.files(path = answerfolders, full.names = TRUE, recursive = FALSE)
# Filter folders from files
folderList <- folderList[sapply(folderList, function(x) file.info(x)$isdir)]

# Dataframe to collect all answers from all files
allanswers <- data.frame(ID=integer())

# Loop through all folders
for (folderPath in folderList){
  notfound <- 0
  
  # Dataframe to collect answers from actual file
  mergeData <- data.frame(ID=integer())
  
  # Navigate to every file in every file in actual folder
  modellfolder <- basename(folderPath)
  directory <- paste0(answerfolders,modellfolder)
  tsv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  # Collect all answers from files
  expAAnswers_list <- lapply(tsv_files, function(x) read.delim(x, header = FALSE))
  
  # Extract relevant information from answers into target structure
  for (answerList in expAAnswers_list) {
    toMergeList <- data.frame(
      ID = answerList$V1, 
      V3 = answerList$V3
    )
    names(toMergeList)[names(toMergeList) == "V3"] <- paste0(modellfolder,"_Completion")
    toMergeList$V5 <- answerList$V5 
    names(toMergeList)[names(toMergeList) == "V5"] <- paste0(modellfolder,"_Antecedens")
    mergeData <- rbind(mergeData, toMergeList)
  }
  # Aggregate all information from all answers
  allanswers <- merge(allanswers, mergeData, by="ID", all=TRUE)
  print(paste(modellfolder, ":", notfound ))
}
# Merge answers into experiment data
expAData <- merge(expAData, allanswers, by = "ID", all = TRUE)

# Try to identify antecedences within the answers
antecedens_columns <- grep("Antecedens$", names(expAData), value = TRUE)
for (col in antecedens_columns) {
  expAData[[col]] <- gsub('"', '', expAData[[col]])
  expAData[[col]] <- mapply(check_and_return, expAData$RE1, expAData$RE2, expAData[[col]])
}

# Split by verb classes
expA1Data <- subset(expAData, ExperimentID != "A2")
expA2Data <- subset(expAData, ExperimentID != "A1")

# Save data with all answers from the llms
saveRDS(expA1Data, file="Data/ExpA1DataAnswers.rds")
saveRDS(expA2Data, file="Data/ExpA2DataAnswers.rds")

