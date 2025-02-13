# writes rda files to csv for further processing

loaded_longdata_all <- load(file = "exp2_analysis/data_objects/longdata_all.rda") #nolint
loaded_longdata_exp <- load(file = "exp2_analysis/data_objects/longdata_exp.rda") #nolint

# files exist now
if(FALSE){
    for (obj_name in loaded_longdata_exp) {
    obj <- get(obj_name)  # Retrieve the actual object

    # Check if the object is a data frame (to avoid errors with non-dataframe objects) #nolint
    if (is.data.frame(obj)) {
        write.csv(obj, paste0("exp2_analysis/reproduction_of_results", obj_name, ".csv"), row.names = FALSE) #nolint
    } else {
        cat("Skipping non-dataframe object:", obj_name, "\n")
    }
}
}


# longdata_exp.rda -> longdata_exp.csv (in reproduction_of_results/)
# ------------------------------------------------------------------
# reproduction of results as seen in paper

library(dplyr)

df <- read.csv("exp2_analysis/reproduction_of_results/longdata_exp.csv") #nolint

agent_df <- df %>% filter(Antecedent == "AGENT") # agent
recipient_df <- df %>% filter(Antecedent == "RECIPIENT") # recipient
patient_df <- df %>% filter(Antecedent == "PATIENT") # patient

# ------------------------------------------------------------------

# AGENT results
agent_er <- agent_df %>% filter(Pronoun == "ER")
agent_der <- agent_df %>% filter(Pronoun == "DER")
agent_dieser <- agent_df %>% filter(Pronoun == "DIESER")

avg_agent_er <- agent_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_agent_der <- agent_der %>% summarise(avg = mean(score, na.rm = TRUE))
avg_agent_dieser <- agent_dieser %>% summarise(avg = mean(score, na.rm = TRUE))

result_df_np1 <- data.frame(
  AGENT = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_agent_er$avg, avg_agent_der$avg, avg_agent_dieser$avg)
)

# PATIENT results
patient_er <- patient_df %>% filter(Pronoun == "ER")
patient_der <- patient_df %>% filter(Pronoun == "DER")
patient_dieser <- patient_df %>% filter(Pronoun == "DIESER")

avg_patient_er <- patient_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_patient_der <- patient_der %>% summarise(avg = mean(score, na.rm = TRUE))
avg_patient_dieser <- patient_dieser %>% summarise(avg = mean(score, na.rm = TRUE)) #nolint

result_df_np2 <- data.frame(
  PATIENT = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_patient_er$avg, avg_patient_der$avg, avg_patient_dieser$avg) #nolint
)

# RECIPIENT results
recipient_er <- recipient_df %>% filter(Pronoun == "ER")
recipient_der <- recipient_df %>% filter(Pronoun == "DER")
recipient_dieser <- recipient_df %>% filter(Pronoun == "DIESER")

avg_recipient_er <- recipient_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_recipient_der <- recipient_der %>% summarise(avg = mean(score, na.rm = TRUE)) #nolint
avg_recipient_dieser <- recipient_dieser %>% summarise(avg = mean(score, na.rm = TRUE)) #nolint

result_df_np3 <- data.frame(
  RECIPIENT = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_recipient_er$avg, avg_recipient_der$avg, avg_recipient_dieser$avg) #nolint
)

print(result_df_np1)
print(result_df_np2)
print(result_df_np3)