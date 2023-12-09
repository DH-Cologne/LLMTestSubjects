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

# Dataframe to collect all answers from all files
allanswers <- data.frame(ID=integer())

# Loop through all folders
for (folderPath in folderList){
  
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
    toMergeList$V5 <- extractLastWord(answerList$V5)
    names(toMergeList)[names(toMergeList) == "V5"] <- paste0(modellfolder,"_Antecedens")
    mergeData <- rbind(mergeData, toMergeList)
  }
  # Aggregate all information from all answers
  allanswers <- merge(allanswers, mergeData, by="ID", all=TRUE)
}
# Merge answers into experiment data
expAData <- merge(expAData, allanswers, by = "ID", all = TRUE)

write.table(allanswers, file="temp/expA.csv", sep="\t")

saveRDS(expAData, file="Data/ExpADataAnswers.rds")

