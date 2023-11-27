rm(list=ls())
# Function to extract the last word of a string
extractLastWord <- function(s) {
  # Regular expression that removes everything except the last word
  lastWord <- sub(".*\\s(\\w+)[^\\w]*$", "\\1", s)
  return(lastWord)
}

# Read original Dataframe
expBData <- readRDS("Data/expBData.rds")

answerfolders = "LLMAnswers/Ratings/"
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
      V3 = answerList$V3
    )
    names(toMergeList)[names(toMergeList) == "V3"] <- paste0(modellfolder,"_Rating")
    # Zusammenführen der DataFrames basierend auf der ID
    expBData <- merge(expBData, toMergeList, by = "ID", all.x = TRUE)
  }
}
#saveRDS(expBData, file="Data/ExpBDataAnswers.rds")
