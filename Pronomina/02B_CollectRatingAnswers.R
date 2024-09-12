# This code can be used to read out the answers given by the LLMs and aggregates 
# them together with the experiment data (ExpBData) within two a new data frames 
# (ExpB1DataAnswers and ExpB2DataAnswers).

# The study examined ditransitive verbs (ExpB1) and benefactive verbs (ExpB2). 
# Ditransitive verbs involve three arguments (agent, recipient, and patient), 
# such as "give" or "send," where a possession is transferred. 
# Benefactive verbs involve actions performed for the benefit of a third party, 
# like "buy" in "buy a gift for someone," involving an agent, benefactive 
# recipient, and patient.

rm(list=ls())

source("99_Utils.R")

# Read original Dataframe
expBData <- readRDS("Data/ExpBData.rds")
answerfolders = "LLMAnswers/Ratings/"

# List files in destination folder 
folderList <- list.files(path = answerfolders, full.names = TRUE, recursive = FALSE)
# Filter folders from files
folderList <- folderList[sapply(folderList, function(x) file.info(x)$isdir)]

# Dataframe to collect all answers from all files
allanswers <- data.frame(ID=integer())

# ToDo loop over all answerfolders
for (folderPath in folderList) {
  
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
    names(toMergeList)[names(toMergeList) == "V3"] <- paste0(modellfolder,"_Rating")
    mergeData <- rbind(mergeData, toMergeList)
  }
  # Aggregate all information from all answers
  allanswers <- merge(allanswers, mergeData, by="ID", all=TRUE)
}

# Merge answers into experiment data
expBData <- merge(expBData, allanswers, by = "ID", all = TRUE)
expB1Data <- subset(expBData, ExperimentID != "B2")
expB2Data <- subset(expBData, ExperimentID != "B1")

# Save data with all answers from the llms
saveRDS(expB1Data, file="Data/ExpB1DataAnswers.rds")
saveRDS(expB2Data, file="Data/ExpB2DataAnswers.rds")


#write.table(allanswers, file="temp/expB.csv", sep="\t")

# Save data with all answers from the llms
# saveRDS(expBData, file="Data/ExpBDataAnswers.rds")

