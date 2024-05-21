# The code in this file reads the data obtained in the experiments of 
# Patterson and Schumacher (2021, further [ExpB]) and Patterson et al. (2022, further [ExpA]). 
# [You can find the weblinks in our Github repository]     
# The code generates two data frames (ExpADate, ExpBDate) that we use to generate 
# our prompts for the large language models (folder ExperimentParticipantsLists) 
# and exports them to RDS files.
# These files allow you to feed the LLMs and use our code 
# (02/03 A/B) to analyse your results without having to rely on the original data.   

rm(list=ls())
# Read and preprocess RDS data
combdata <- readRDS("Data/OriginalData/CombinedData.rds")

# Delete experiments not needed for the analysis
combdata <- subset(combdata, !ExperimentID %in% c("B3", "B4", "B5"))

# Factorize suitable variables
combdata$ItemNumber <- factor(combdata$ItemNumber)
combdata$ArgumentOrder <- factor(combdata$ArgumentOrder)
combdata$ExperimentID <- factor(combdata$ExperimentID)
combdata$IsFiller <- factor(combdata$IsFiller)
combdata$ParticipantID <- factor(combdata$ParticipantID)
combdata$VerbType <- factor(combdata$VerbType)
combdata$ContextValency <- factor(combdata$ContextValency)
combdata$RE1gennum <- factor(combdata$RE1gennum)
combdata$RE1case <- factor(combdata$RE1case)
combdata$RE1proto <- factor(combdata$RE1proto)
combdata$RE1art <- factor(combdata$RE1art)
combdata$RE1gram <- factor(combdata$RE1gram)
combdata$RE2gennum <- factor(combdata$RE2gennum)
combdata$RE2case <- factor(combdata$RE2case)
combdata$RE2proto <- factor(combdata$RE2proto)
combdata$RE2art <- factor(combdata$RE2art)
combdata$RE2gram <- factor(combdata$RE2gram)
combdata$RE3gennum <- factor(combdata$RE3gennum)
combdata$RE3case <- factor(combdata$RE3case)
combdata$RE3proto <- factor(combdata$RE3proto)
combdata$RE3art <- factor(combdata$RE3art)
combdata$RE3gram <- factor(combdata$RE3gram)
combdata$REPgennum <- factor(combdata$REPgennum)
combdata$REPcase <- factor(combdata$REPcase)
combdata$REPproto <- NULL
combdata$REPart <- NULL
combdata$REPgram <- NULL
combdata$REPtype <- factor(combdata$REPtype)
combdata$zR <- factor(combdata$zR)
combdata$RE1role <- factor(combdata$RE1role)
combdata$RE2role <- factor(combdata$RE2role)
combdata$RE3role <- factor(combdata$RE3role)
combdata$IsFiller <- NULL
combdata$Ct <- NULL
combdata$AntecedentE <- factor(combdata$AntecedentE)
combdata$AntecedentAnswer <- factor(combdata$AntecedentAnswer)
combdata$Rating7 <- as.numeric(combdata$Rating7)
combdata$Rating7i <- NULL

# add IDs to the first column
startID <- 100001
zeilenIDs <- startID + seq_len(nrow(combdata)) - 1
combdata$ID <- paste("ID", zeilenIDs, sep = "_")
combdata <- combdata[c("ID", setdiff(names(combdata), "ID"))]

# replace "die" with "Die" etc
combdata$REP <- sub("^d", "D", combdata$REP)

# separate data by experiments
expAData <- combdata[grepl("^A", combdata$ExperimentID),]
expBData <- combdata[grepl("^B", combdata$ExperimentID),]


## preprocess expAData

# add annotation question to prompt
extendedPrompt <- paste0("\tAuf was bezieht sich das \"", expAData$REP, "\" in dem Satz, den Sie ergänzt haben?")
expAData$TargetPrompt <- paste0(expAData$TargetPrompt, extendedPrompt) 
expAData$TargetPrompt


## preprocess expBData

# delete empty rows
expBData <- expBData[!is.na(expBData$ContextS), ]
expBData <- expBData[!is.na(expBData$TargetPrompt), ]


# Replace "Er/der/dieser" bzw. "Sie/die/diese" with Value of REP
replaceFunction <- function(target, rep) {
  target <- sub("^Er/der/dieser", rep, target)
  target <- sub("^Sie/die/diese", rep, target)
  return(target)
}
expBData$TargetPrompt <- mapply(replaceFunction, expBData$TargetPrompt, expBData$REP)
expBData$TargetPrompt <- paste0(expBData$TargetPrompt, " \\nWie klingt der Text? (1=sehr seltsam, 7=perfekt)")

## recombine Data to generate prompts and participants lists
combdata <- rbind(expAData, expBData)

# Generate full prompts
combdata$FullPrompt <- paste(combdata$ContextS, combdata$TargetPrompt, sep = ". ")
combdata$FullPrompt <- paste(combdata$ID, combdata$FullPrompt, sep = "\t")

# Scramble data
set.seed(4711)
samplemix <- sample(nrow(combdata))
scambleddata <- combdata[samplemix, ]

# Aggregate prompts for each ParticipantID
participantsLists <- aggregate(FullPrompt ~ ParticipantID, data = scambleddata, paste, collapse = "\n")

# Write csv file for each participant
for (i in 1:nrow(participantsLists)) {
  participantID <- participantsLists$ParticipantID[i]
  content <- participantsLists$FullPrompt[i]
  filename <- paste0("ExperimentParticipantsLists/", participantID, ".csv")
  writeLines(content, filename)
}

# delete empty columns
expAData <- expAData[, colSums(is.na(expAData)) != nrow(expAData)]
expBData <- expBData[, colSums(is.na(expBData)) != nrow(expBData)]

# export data
saveRDS(expAData, file="Data/ExpAData.rds")
saveRDS(expBData, file="Data/ExpBData.rds")
