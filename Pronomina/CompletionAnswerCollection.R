rm(list=ls())
# Function to extract the last word of a string
extractLastWord <- function(s) {
  # Regular expression that removes everything except the last word
  lastWord <- sub(".*\\s(\\w+)[^\\w]*$", "\\1", s)
  return(lastWord)
}

# Read original Dataframe
expAData <- readRDS("Data/expAData.rds")

answerfolders = "LLMAnswers/Completions/"

# List files in destination folder 
folderList <- list.files(path = answerfolders, full.names = TRUE, recursive = FALSE)
# Filter folders from files
folderList <- folderList[sapply(folderList, function(x) file.info(x)$isdir)]

# ToDo loop over all answerfolders
for (folderPath in folderList) {
  modellfolder <- basename(folderPath)

  directory <- paste0(answerfolders,modellfolder)
  tsv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  expAAnswers_list <- lapply(tsv_files, function(x) read.delim(x, header = FALSE))
  
  for (answerList in expAAnswers_list) {
    toMergeList <- data.frame(
      ID = answerList$V1, 
      V4 = answerList$V4
    )
    names(toMergeList)[names(toMergeList) == "V4"] <- paste0(modellfolder,"_Completion")
    toMergeList$V5 <- extractLastWord(answerList$V5)
    names(toMergeList)[names(toMergeList) == "V5"] <- paste0(modellfolder,"_Antecedens")
    
    # Zusammenführen der DataFrames basierend auf der ID
    expAData <- merge(expAData, toMergeList, by = "ID", all.x = TRUE)
  }
}
#saveRDS(expAData, file="Data/ExpADataAnswers.rds")

