
### EXP1
### RATING QUESTIONNAIRE
### TESTING RESOLUTION OF GERMAN PERSONAL AND DEMONSTRATIVE PRONOUNS IN DITRANSITIVE CONTEXTS

## RUN IN MARCH 2019, USING QUALTRICS
### PARTICIPANTS RECRUITED VIA PROLIFIC.AC

### this script analyses the (already tidied) data

### condition labels: NP1 = Agent, NP2 = Recipient, NP3 = Patient

## datl is the data frame in long format, including experimental and filler items
## datl_e is a subset, containing only experimental items
## datl_f is a subset, containing only filler items

# ----------------------------------------------------------------------------
##### 1. SET-UP AND LOAD DATA #####
# ----------------------------------------------------------------------------

### set working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))                               

###set data objects location
ob_loc <- file.path(getwd(), "data_objects")

###set output location
output_loc <- file.path(getwd(), "outputs")

###set data location
#data_loc <- file.path(getwd(), "data_files")

### load packages
library(tidyverse)
library(lmerTest)
library(RePsychLing)    

## load data files
load(file.path(ob_loc, "longdata_all.rda"))
load(file.path(ob_loc, "longdata_exp.rda"))
load(file.path(ob_loc, "longdata_fillers.rda"))

## create empty list for participant biog summary
participant_bio <- list()

# ----------------------------------------------------------------------------
##### 2. ANALYSE BIOG DATA OF PARTICIPANTS #####
# ----------------------------------------------------------------------------

## check if all participants gave consent
consent_check <- datl %>%
  group_by(subj) %>%
  summarise(consent = sum(consent)/72)

## check if all participants finished
finished_check <- datl %>%
  group_by(subj) %>%
  summarise(finished = sum(Finished)/72)

## combine tables and save output
finished_and_consent <- as.data.frame(inner_join(consent_check, finished_check))

write.table(finished_and_consent, 
            file = file.path(output_loc, "participant_check_finished_consent.txt"),
            sep = "\t",
            row.names = FALSE)

## age
meanage_all <- mean(datl$Age)
rangeage_all <- range(datl$Age)

## age stats of 58 participants remaining in analysis
meanage_58 <- mean(datl[datl$subj != "6" & datl$subj != "35",]$Age)
rangeage_58 <- range(datl[datl$subj != "6" & datl$subj != "35",]$Age)

## write age data to file
participant_bio[['mean_age_60_participants']] <- meanage_all
participant_bio[['age_range_60_participants']] <- rangeage_all
participant_bio[['mean_age_58_participants']] <- meanage_58
participant_bio[['age_range_58_participants']] <- rangeage_58


## gender
gender_summary <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(Sex_c)
gender_summary$n_participants <- gender_summary$n/ 72

participant_bio[['gender_summary_key']] <- "M = male, W = female, NB = non-binary"
participant_bio[['gender_summary']] <- gender_summary

## native language
## response "1" = participant confirmed that native lang was German
natlang_summary <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(Native_German)
natlang_summary$n_participants <- natlang_summary$n/ 72
## all participants responded "1"

participant_bio[['native_lang_key']] <- "response 1 = participant confirmed that native lang was German"
participant_bio[['native_lang']] <- natlang_summary

## language disorders
## response "2" = participant reported no language disorders
lang_dis <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(lang_disorder)
lang_dis$n_participants <- lang_dis$n/ 72
## all participants responded "2"

participant_bio[['lang_disorders_key']] <- "response 2 = participant reported no language disorders"
participant_bio[['lang_disorders']] <- lang_dis

## bilingual participants
## response "1" = bilingual; "2" = not bilingual
biling_summary <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(biling)
biling_summary$n_participants <- biling_summary$n/ 72

participant_bio[['bilingual_participants_key']] <- "response 1 = bilingual; 2 = not bilingual"
participant_bio[['bilingual_participants']] <- biling_summary

## bilingual language details
biling_detail <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(biling_details)
biling_detail$n_participants <- biling_detail$n/ 72

participant_bio[['bilingual_language_details']] <- biling_detail

## save participant bio info
capture.output(participant_bio, file=file.path(output_loc, "participant_bio_info.txt"))

# ----------------------------------------------------------------------------
##### 3. CHECK LIST AND ITEM DISTRIBUTION #####
# ----------------------------------------------------------------------------

## check distribution of lists over participants
list_summary <- datl %>%
  filter(subj != "6" & subj != "35") %>%     ## exclude 2 participants
  count(List)
list_summary$n_participants <- list_summary$n/ 72

## save output to table
write.table(list_summary, 
            file = file.path(output_loc, "list_summary.txt"),
            sep = "\t",
            row.names = FALSE)

# ----------------------------------------------------------------------------
##### 4. SANITY CHECK TASK #####
# ----------------------------------------------------------------------------

## check whether "weird" fillers received overall lower scores than "normal" fillers
filler_comparison_overall <- datl_f %>%
  group_by(weird_filler) %>%
  summarise(mean_score = mean(score), 
            sd_score = sd(score),
            mean_z_score = mean(zscore),
            sd_z_score = sd(zscore))

## output table to file
write.table(filler_comparison_overall, 
            file = file.path(output_loc, "fillers_comparison_overall.txt"),
            sep = "\t",
            row.names = FALSE)

## check whether individual participants rated weird fillers lower than normal filler: exclusion criterion
## descriptives per subject
filler_descriptives_subs <- datl_f %>%
  group_by(subj, weird_filler) %>%
  summarise(mean_score = mean(score), 
            sd_score = sd(score),
            mean_z_score = mean(zscore),
            sd_z_score = sd(zscore))

## output table to file
write.table(filler_descriptives_subs, 
            file = file.path(output_loc, "fillers_zscore_persubj.txt"),
            sep = "\t",
            row.names = FALSE)

## weird filler analysis (compare zscores for weird to normal)
zscore_fillers_mod <- lmer(zscore ~ weird_filler  
                         +         (1    | subj) +
                           +         (1    | item_nr),
                         data=datl_f[datl_f$subj != "6" & datl_f$subj != "35",])

summary(zscore_fillers_mod)


## compare raw scores for er to raw scores for normal fillers
fill_exper_comp <- lmer(score ~ qtype
                    + (1 | subj),
                      data=datl[datl$subj != "6" 
                                & datl$subj != "35"
                                & datl$weird_filler != "1"
                                & datl$Pronoun != "DIESER"
                                & datl$Pronoun != "DER",])

summary(fill_exp_comp)

## compare scores for fillers to scores for experimental items
fillall_expall_comp <- lmer(score ~ qtype
                        + (1 | subj),
                        data=datl[datl$subj != "6" 
                                  & datl$subj != "35",])

summary(fillall_expall_comp)


# ----------------------------------------------------------------------------
##### 5. DESCRIPTIVE STATS #####
# ----------------------------------------------------------------------------

## means and sds for experimental items
overall_means_sds <- datl_e %>%
  filter(subj != "6" & subj != "35") %>%       ## exclude two participants
  group_by(Pronoun, Antecedent) %>%
  summarise(mean_raw = mean(score), 
            sd_raw = sd(score), 
            mean_zscore = mean(zscore), 
            sd_zscore = sd(zscore))

## output table to file
write.table(overall_means_sds, 
            file = file.path(output_loc, "overall_means_sds.txt"),
            sep = "\t",
            row.names = FALSE)


# ----------------------------------------------------------------------------
##### 6. INFERENTIAL STATS - PRIMARY ANALYSIS #####
# ----------------------------------------------------------------------------

# relevel Pronoun factor so that ER is baseline
datl_e <- within(datl_e, {
  Pronoun <- relevel(Pronoun, ref = "ER")
  print(attributes(Pronoun))
})


#### overall model, with and without interaction
### not included in pre-registered experiment, but requested by reviewer 1 to justify splitting data

### sum code variables
datl_e <- within(datl_e, {
  pron.sum <- C(Pronoun, sum)
  print(attributes(pron.sum))
})

datl_e <- within(datl_e, {
  ant.sum <- C(Antecedent, sum)
  print(attributes(ant.sum))
})


## started with full random effects structure ((1 + pron.sum * ant.sum | subj) + 
## (1 + pron.sum * ant.sum  | item_nr))
## resulted in singular fit. Progressively removed random effects 
## until singularity was avoided and had no convergence problems
zscore_model_all_withint <- lmer(zscore ~ pron.sum * ant.sum  
                                 +         (1   | subj) 
                                 +         (1     | item_nr) ,
                                 data=datl_e[datl_e$subj != "6" 
                                             & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_all_withint)
summary(rePCA(zscore_model_all_withint))

## summary
summary(zscore_model_all_withint)

### now take same model (with same random structure) and just remove the interaction
zscore_model_all_noint <- lmer(zscore ~ pron.sum + ant.sum  
                               +         (1     | subj) 
                               +         (1     | item_nr) ,
                               data=datl_e[datl_e$subj != "6" 
                                           & datl_e$subj != "35",])


## summary
summary(zscore_model_all_noint)

## compare models with likelihood ratio test
overall_model_comp <- anova(zscore_model_all_noint, zscore_model_all_withint)
## model NP1
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_NP1 <- lmer(zscore ~ Pronoun  
                                   +         (1    | subj) +
                                     +         (1    | item_nr),
                                   data=datl_e[datl_e$Antecedent == "NP1" & datl_e$subj != "6" & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_NP1)
summary(rePCA(zscore_model_NP1))

## summary
summary(zscore_model_NP1)

## model NP2
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_NP2 <- lmer(zscore ~ Pronoun  
                                   +         (1   | subj) +
                                     +         (1   | item_nr),
                                   data=datl_e[datl_e$Antecedent == "NP2" & datl_e$subj != "6" & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_NP2)
summary(rePCA(zscore_model_NP2))

## summary
summary(zscore_model_NP2)


## model NP3
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_NP3 <- lmer(zscore ~ Pronoun 
                                   +         (1    | subj) +
                                     +         (1   | item_nr),
                                   data=datl_e[datl_e$Antecedent == "NP3" & datl_e$subj != "6" & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_NP3)
summary(rePCA(zscore_model_NP3))

## summary
summary(zscore_model_NP3)

# ----------------------------------------------------------------------------
##### 7. INFERENTIAL STATS - SECONDARY ANALYSIS #####
# ----------------------------------------------------------------------------

## compare ER across three NPs
## started with full random effects structure ((1 + Antecedent | subj) + (1 + Antecedent  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_ER <- lmer(zscore ~ Antecedent 
                         +         (1  | subj) +
                           +         (1 + Antecedent  | item_nr),
                         data=datl_e[datl_e$Pronoun == "ER" & datl_e$subj != "6" & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_ER)
summary(rePCA(zscore_model_ER))

## summary
summary(zscore_model_ER)


## compare DER AND DIESER in lower prominence conditions
## started with full random effects structure ((1 + Pronoun * Antecedent | subj) + (1 + Pronoun * Antecedent  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_dems <- lmer(zscore ~ Pronoun * Antecedent 
                        +         (1 + Pronoun   | subj) +
                          +         (1   | item_nr),
                        data=datl_e[datl_e$Pronoun != "ER"
                                    & datl_e$Antecedent != "NP1"
                                    & datl_e$subj != "6" 
                                    & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_dems)
summary(rePCA(zscore_model_dems))

## summary
summary(zscore_model_dems)


# ----------------------------------------------------------------------------
##### 8. EXPLORATORY: dieser as baseline for Patient #####
# ----------------------------------------------------------------------------

# relevel Pronoun factor so that dieser is baseline
datl_e <- within(datl_e, {
  Pronoun <- relevel(Pronoun, ref = "DIESER")
  print(attributes(Pronoun))
})

## model NP3
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_NP3dieser <- lmer(zscore ~ Pronoun  
                               +         (1    | subj) +
                                 +         (1   | item_nr),
                               data=datl_e[datl_e$Antecedent == "NP3" & datl_e$subj != "6" & datl_e$subj != "35",])

## test for singular fit
isSingular(zscore_model_NP3dieser)
summary(rePCA(zscore_model_NP3dieser))

## summary
summary(zscore_model_NP3dieser)

# ----------------------------------------------------------------------------
##### 9. EXPLORATORY: forward difference coding #####
# ----------------------------------------------------------------------------
## forward difference (compares each level with previous one) to get graded response for demonstratives


datl_dem <- datl_e %>%
  filter(Pronoun != "ER")
## drop unused levels
datl_dem$Pronoun <- droplevels(datl_dem$Pronoun)

library(MASS)

Antecedent.for.matrix <- contr.sdif(datl_dem$Antecedent, n=3)
contrasts(datl_dem$Antecedent) <- Antecedent.for.matrix
print(attributes(datl_dem$Antecedent))

## model with forward contrasts
zscore_model_dems_for <- lmer(zscore ~ Pronoun * Antecedent
                              +         (1    | subj) +
                                +         (1   | item_nr),
                              data=datl_dem[datl_dem$subj != "6" 
                                            & datl_dem$subj != "35",])

## test for singular fit
isSingular(zscore_model_dems_for)
summary(rePCA(zscore_model_dems_for))

## summary
summary(zscore_model_dems_for)


# ----------------------------------------------------------------------------
##### 10. WRITE MODEL OUTPUTS TO FILE AND SAVE MODELS #####
# ----------------------------------------------------------------------------

## create empty list in which to send model outputs
stats <- list()

## send model outputs to stats
stats[['fillers_model']] <- summary(zscore_fillers_mod)
stats[['overall_with_interaction']] <- summary(zscore_model_all_withint)
stats[['overall_without_interaction']] <- summary(zscore_model_all_noint)
stats[['overall_model_comparison']] <- overall_model_comp
stats[['NP1_model']] <- summary(zscore_model_NP1)
stats[['NP2_model']] <- summary(zscore_model_NP2)
stats[['NP3_model']] <- summary(zscore_model_NP3)
stats[['ER_model']] <- summary(zscore_model_ER)
stats[['dems_model']] <- summary(zscore_model_dems)
stats[['dieserNP3']] <- summary(zscore_model_NP3dieser)
stats[['dems_forward_coding']] <- summary(zscore_model_dems_for)

## write stats to text file
capture.output(stats, file = file.path(output_loc, "model_outputs_exp.txt"))

## save models as objects
save(zscore_model_all_withint, file = file.path(output_loc, "zscore_model_all_withint.rda"))
save(zscore_model_all_noint, file = file.path(output_loc, "zscore_model_all_noint.rda"))
save(zscore_model_NP1, file = file.path(output_loc, "zscore_model_NP1.rda"))
save(zscore_model_NP2, file = file.path(output_loc, "zscore_model_NP2.rda"))
save(zscore_model_NP3, file = file.path(output_loc, "zscore_model_NP3.rda"))
save(zscore_model_ER, file = file.path(output_loc, "zscore_model_ER.rda"))
save(zscore_model_dems, file = file.path(output_loc, "zscore_model_dems.rda"))
save(zscore_model_NP3dieser, file = file.path(output_loc, "zscore_model_NP3dieser.rda"))
save(zscore_model_dems_for, file = file.path(output_loc, "zscore_model_dems_for.rda"))


# ----------------------------------------------------------------------------
##### 11. SAVE SESSION INFO #####
# ----------------------------------------------------------------------------

## session info
# extract session info and store as .txt file
writeLines(capture.output(sessionInfo()), file.path(output_loc, "sessionInfo_WP3_Exp1_Analysis.txt"))
