
### C07 WP3 EXP2
### RATING QUESTIONNAIRE
### TESTING RESOLUTION OF GERMAN PERSONAL AND DEMONSTRATIVE PRONOUNS IN DITRANSITIVE CONTEXTS
### FOLLOW UP EXPERIMENT TO EXP 1; SWAPPED ORDER OF RECIPIENT AND PATIENT ARGUMENTS

## RUN IN MAY 2019, USING QUALTRICS
### PARTICIPANTS RECRUITED VIA PROLIFIC.AC

### this script analyses the (already tidied) data


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
data_loc <- file.path(getwd(), "data_files")

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
  count(consent)

## check if all participants finished
finished_check <- datl %>%
  group_by(subj) %>%
  count(Finished)

## combine tables and save output
finished_and_consent <- as.data.frame(inner_join(consent_check, finished_check))

write.table(finished_and_consent, 
            file = file.path(output_loc, "participant_check_finished_consent.txt"),
            sep = "\t",
            row.names = FALSE)

## AGE
## age stats of whole group, before exclusions
meanage_60_part <- mean(datl$Age)
rangeage_60_part <- range(datl$Age)

## write age data to file
participant_bio[['mean_age_participants_60part']] <- meanage_60_part
participant_bio[['age_range_participants_60part']] <- rangeage_60_part

## age stats of 57 participants remaining in analysis
meanage_57_part <- mean(datl[datl$subj != "52" & datl$subj != "40" & datl$subj != "31",]$Age)
rangeage_57_part <- range(datl[datl$subj != "52" & datl$subj != "40" & datl$subj != "31",]$Age)

## write age data to file
participant_bio[['mean_age_participants_57part']] <- meanage_57_part
participant_bio[['age_range_participants_57part']] <- rangeage_57_part


## gender
gender_summary <- datl %>%
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants
  count(Sex_c)
gender_summary$n_participants <- gender_summary$n/ 81

participant_bio[['gender_summary_key']] <- "M = male, W = female"
participant_bio[['gender_summary']] <- gender_summary

## native language
## response "Ja" = participant confirmed that native lang was German
natlang_summary <- datl %>%
  count(Native_German)
natlang_summary$n_participants <- natlang_summary$n/ 81
## 2 participants responded no - they will be excluded

participant_bio[['native_lang_key']] <- "response Ja = participant confirmed that native lang was German"
participant_bio[['native_lang']] <- natlang_summary

## language disorders
## response "Nein" = participant reported no language disorders
lang_dis <- datl %>%
  count(lang_disorder)
lang_dis$n_participants <- lang_dis$n/ 81
## all participants responded "Nein"

participant_bio[['lang_disorders_key']] <- "response Nein = participant reported no language disorders"
participant_bio[['lang_disorders']] <- lang_dis

## bilingual participants
## response "Ja" = bilingual; "Nein" = not bilingual
biling_summary <- datl %>%
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants  
  count(biling)
biling_summary$n_participants <- biling_summary$n/ 81

participant_bio[['bilingual_participants_key']] <- "response Ja = bilingual; Nein = not bilingual"
participant_bio[['bilingual_participants']] <- biling_summary

## bilingual language details
biling_detail <- datl %>%
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants
  count(biling_details)
biling_detail$n_participants <- biling_detail$n/ 81

participant_bio[['bilingual_language_details']] <- biling_detail

## save participant bio info
capture.output(participant_bio, file=file.path(output_loc, "participant_bio_info.txt"))

# ----------------------------------------------------------------------------
##### 3. CHECK LIST AND ITEM DISTRIBUTION #####
# ----------------------------------------------------------------------------

## check distribution of lists over participants
list_summary <- datl %>%
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants
  count(List)
list_summary$n_participants <- list_summary$n/ 81

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
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants
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
                           data=datl_f[datl_f$subj != "52" & datl_f$subj != "40" & datl_f$subj != "31",])

summary(zscore_fillers_mod)

# ----------------------------------------------------------------------------
##### 5. DESCRIPTIVE STATS #####
# ----------------------------------------------------------------------------

## means and sds for experimental items
overall_means_sds <- datl_e %>%
  filter(subj != "52" & subj != "40" & subj != "31") %>%       ## exclude three participants
  group_by(Pronoun, Antecedent) %>%
  summarise(mean_raw = mean(score), 
            sd_raw = sd(score), 
            mean_zscore = mean(zscore), 
            sd_zscore = sd(zscore)) %>%
  arrange(Antecedent)

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
                                 +         (1    | subj) 
                                 +         (1    | item_nr) ,
                                 data=datl_e[datl_e$subj != "52" 
                                             & datl_e$subj != "40" 
                                             & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_all_withint)
summary(rePCA(zscore_model_all_withint))

## summary
summary(zscore_model_all_withint)

### now take same model (with same random structure) and just remove the interaction
zscore_model_all_noint <- lmer(zscore ~ pron.sum + ant.sum  
                               +         (1     | subj) 
                               +         (1     | item_nr) ,
                               data=datl_e[datl_e$subj != "52" 
                                           & datl_e$subj != "40" 
                                           & datl_e$subj != "31",])


## summary
summary(zscore_model_all_noint)

## compare models with likelihood ratio test
overall_model_comp <- anova(zscore_model_all_noint, zscore_model_all_withint)


## model AGENT
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems

datl_e$subj <- as.factor(as.character(datl_e$subj))

zscore_model_AGENT <- lmer(zscore ~ Pronoun  
                                   +         (1   | subj) +
                                     +         (1    | item_nr),
                                   data=datl_e[datl_e$Antecedent == "AGENT" & datl_e$subj != "52" 
                                               & datl_e$subj != "40" & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_AGENT)
summary(rePCA(zscore_model_AGENT))

## summary
summary(zscore_model_AGENT)



## model PATIENT
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_PATIENT <- lmer(zscore ~ Pronoun  
                                   +         (1   | subj) +
                                     +         (1   | item_nr),
                                   data=datl_e[datl_e$Antecedent == "PATIENT" & datl_e$subj != "52"
                                               & datl_e$subj != "40" & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_PATIENT)
summary(rePCA(zscore_model_PATIENT))

## summary
summary(zscore_model_PATIENT)


## model RECIPIENT
## started with full random effects structure ((1 + Pronoun | subj) + (1 + Pronoun  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_RECIPIENT <- lmer(zscore ~ Pronoun 
                                   +         (1   | subj) +
                                     +         (1 | item_nr),
                                   data=datl_e[datl_e$Antecedent == "RECIPIENT" & datl_e$subj != "52"
                                               & datl_e$subj != "40" & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_RECIPIENT)
summary(rePCA(zscore_model_RECIPIENT))

## summary
summary(zscore_model_RECIPIENT)

# ----------------------------------------------------------------------------
##### 7. INFERENTIAL STATS - SECONDARY ANALYSIS #####
# ----------------------------------------------------------------------------

## compare ER across three NPs
## started with full random effects structure ((1 + Antecedent | subj) + (1 + Antecedent  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_ER <- lmer(zscore ~ Antecedent 
                         +         (1  | subj) +
                           +         (1  | item_nr),
                         data=datl_e[datl_e$Pronoun == "ER" & datl_e$subj != "52"
                                     & datl_e$subj != "40" & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_ER)
summary(rePCA(zscore_model_ER))

## summary
summary(zscore_model_ER)


## compare DER AND DIESER in all antecedent conditions
## started with full random effects structure ((1 + Pronoun * Antecedent | subj) + (1 + Pronoun * Antecedent  | item_nr))
## resulted in singular fit. Progressively removed random effects until singularity was avoided and had no convergence problems
zscore_model_dems <- lmer(zscore ~ Pronoun * Antecedent 
                        +         (1 + Pronoun    | subj) +
                          +         (1    | item_nr),
                        data=datl_e[datl_e$Pronoun != "ER"
                                    & datl_e$subj != "52"
                                    & datl_e$subj != "40" 
                                    & datl_e$subj != "31",])

## test for singular fit
isSingular(zscore_model_dems)
summary(rePCA(zscore_model_dems))

## summary
summary(zscore_model_dems)


# ----------------------------------------------------------------------------
##### 8. INFERENTIAL STATS - EXPLORATORY #####
# ----------------------------------------------------------------------------

### forward difference coding

## reduce data set
datl_dem <- datl_e %>%
  filter(Pronoun != "ER")

## drop unused levels
datl_dem$Pronoun <- droplevels(datl_dem$Pronoun)

## use package 'MASS'
library(MASS)

# forward difference coding
Antecedent.for.matrix <- contr.sdif(datl_dem$Antecedent, n=3)
contrasts(datl_dem$Antecedent) <- Antecedent.for.matrix
print(attributes(datl_dem$Antecedent))

## model with forward contrasts
zscore_model_dems_forw <- lmer(zscore ~ Pronoun * Antecedent
                              +         (1    | subj) +
                                +         (1   | item_nr),
                              data=datl_dem[datl_dem$subj != "52" 
                                            & datl_dem$subj != "40" 
                                            & datl_dem$subj != "31",])

## test for singular fit
isSingular(zscore_model_dems_forw)
summary(rePCA(zscore_model_dems_forw))

## summary
summary(zscore_model_dems_forw)


# ----------------------------------------------------------------------------
##### 9. WRITE MODEL OUTPUTS TO FILE AND SAVE MODELS #####
# ----------------------------------------------------------------------------

## create empty list in which to send model outputs
stats <- list()

## send model outputs to stats
stats[['fillers_model']] <- summary(zscore_fillers_mod)
stats[['overall_with_interaction']] <- summary(zscore_model_all_withint)
stats[['overall_without_interaction']] <- summary(zscore_model_all_noint)
stats[['overall_model_comparison']] <- overall_model_comp
stats[['AGENT_model']] <- summary(zscore_model_AGENT)
stats[['PATIENT_model']] <- summary(zscore_model_PATIENT)
stats[['RECIPIENT_model']] <- summary(zscore_model_RECIPIENT)
stats[['ER_model']] <- summary(zscore_model_ER)
stats[['dems_model']] <- summary(zscore_model_dems)
stats[['dems_model_forward']] <- summary(zscore_model_dems_forw)

## write stats to text file
capture.output(stats, file = file.path(output_loc, "model_outputs_exp.txt"))

## save models as objects
save(zscore_model_all_withint, file = file.path(output_loc, "zscore_model_all_withint.rda"))
save(zscore_model_all_noint, file = file.path(output_loc, "zscore_model_all_noint.rda"))
save(zscore_model_AGENT, file = file.path(output_loc, "zscore_model_AGENT.rda"))
save(zscore_model_PATIENT, file = file.path(output_loc, "zscore_model_PATIENT.rda"))
save(zscore_model_RECIPIENT, file = file.path(output_loc, "zscore_model_RECIPIENT.rda"))
save(zscore_model_ER, file = file.path(output_loc, "zscore_model_ER.rda"))
save(zscore_model_dems, file = file.path(output_loc, "zscore_model_dems.rda"))
save(zscore_model_dems_forw, file = file.path(output_loc, "zscore_model_dems_forw.rda"))

# ----------------------------------------------------------------------------
##### 10. SAVE SESSION INFO #####
# ----------------------------------------------------------------------------

## session info
# extract session info and store as .txt file
writeLines(capture.output(sessionInfo()), file.path(output_loc, "sessionInfo_WP3_Exp2_Analysis.txt"))


