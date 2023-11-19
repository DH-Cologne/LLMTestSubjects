# Read RDS data
combdata <- readRDS("CombinedData.rds")

# Add IDs to the first column
startID <- 100001
zeilenIDs <- startID + seq_len(nrow(combdata)) - 1
combdata$ID <- paste("ID", zeilenIDs, sep = "_")
combdata <- combdata[c("ID", setdiff(names(combdata), "ID"))]

# Replace "die" with "Die" etc
combdata$REP <- sub("^d", "D", combdata$REP)

# Separate data by experiments
expAData <- combdata[grepl("^A", combdata$ExperimentID),]
expBData <- combdata[grepl("^B", combdata$ExperimentID),]

#preprocess expAData - Add annotation question to prompt
expAData$TargetAnswer <- paste0("\tAuf was bezieht sich das \"", expAData$REP, "\" in deinem Antwortsatz?")
expAData$TargetPrompt <- paste(expAData$TargetPrompt, expAData$TargetAnswer) 

# preprocess expBData
expBData <- expBData[!is.na(expBData$ContextS), ]
expBData <- expBData[!is.na(expBData$TargetPrompt), ]
# Replace "Er/der/dieser" bzw. "Sie/die/diese" with Value of REP
replaceFunction <- function(target, rep) {
  target <- sub("^Er/der/dieser", rep, target)
  target <- sub("^Sie/die/diese", rep, target)
  return(target)
}
expBData$TargetPrompt <- mapply(replaceFunction, expBData$TargetPrompt, expBData$REP)

# recombine Data
combdata <- rbind(expAData, expBData)

# Generate full prompts
combdata$FullPrompt <- paste(combdata$ContextS, combdata$TargetPrompt, sep = ". ")
combdata$FullPrompt <- paste(combdata$ID, combdata$FullPrompt, sep = "\t")

# Scramble data
set.seed(1234)
samplemix <- sample(nrow(combdata))
scambleddata <- combdata[samplemix, ]

# Aggregate prompts for each ParticipantID
participantsLists <- aggregate(FullPrompt ~ ParticipantID, data = scambleddata, paste, collapse = "\n")

# Write csv file for each participant
for (i in 1:nrow(participantsLists)) {
  participantID <- participantsLists$ParticipantID[i]
  content <- participantsLists$FullPrompt[i]
  filename <- paste0("ParticipantsLists/", participantID, ".csv")
  writeLines(content, filename)
}

write.csv(combdata, "CombinedData.csv", row.names = FALSE)
