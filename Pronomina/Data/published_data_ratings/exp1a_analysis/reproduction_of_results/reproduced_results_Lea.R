# writes rda files to csv for further processing

loaded_longdata_all <- load(file = "exp1a_analysis/data_objects/longdata_all.rda")
loaded_longdata_exp <- load(file = "exp1a_analysis/data_objects/longdata_exp.rda")

# files exist now
if (FALSE) {
    for (obj_name in loaded_longdata_exp) {
    obj <- get(obj_name)  # Retrieve the actual object

    # Check if the object is a data frame (to avoid errors with non-dataframe objects) #nolint
    if (is.data.frame(obj)) {
        write.csv(obj, paste0("exp1a_analysis/reproduction_of_results/", obj_name, ".csv"), row.names = FALSE) #nolint
    } else {
        cat("Skipping non-dataframe object:", obj_name, "\n")
    }
}
}

# longdata_exp.rda -> longdata_exp.csv (in reproduction_of_results/)
# ------------------------------------------------------------------
# reproduction of results as seen in paper

library(dplyr)

df <- read.csv("exp1a_analysis/reproduction_of_results/longdata_exp.csv")

np1_df <- df %>% filter(Antecedent == "NP1") # agent
np2_df <- df %>% filter(Antecedent == "NP2") # recipient
np3_df <- df %>% filter(Antecedent == "NP3") # patient

# ------------------------------------------------------------------

# NP1 results
np1_er <- np1_df %>% filter(Pronoun == "ER")
np1_der <- np1_df %>% filter(Pronoun == "DER")
np1_dieser <- np1_df %>% filter(Pronoun == "DIESER")

avg_np1_er <- np1_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np1_der <- np1_der %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np1_dieser <- np1_dieser %>% summarise(avg = mean(score, na.rm = TRUE))

result_df_np1 <- data.frame(
  NP1 = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_np1_er$avg, avg_np1_der$avg, avg_np1_dieser$avg)
)

# NP2 results
np2_er <- np2_df %>% filter(Pronoun == "ER")
np2_der <- np2_df %>% filter(Pronoun == "DER")
np2_dieser <- np2_df %>% filter(Pronoun == "DIESER")

avg_np2_er <- np2_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np2_der <- np2_der %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np2_dieser <- np2_dieser %>% summarise(avg = mean(score, na.rm = TRUE))

result_df_np2 <- data.frame(
  NP2 = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_np2_er$avg, avg_np2_der$avg, avg_np2_dieser$avg)
)

# NP3 results
np3_er <- np3_df %>% filter(Pronoun == "ER")
np3_der <- np3_df %>% filter(Pronoun == "DER")
np3_dieser <- np3_df %>% filter(Pronoun == "DIESER")

avg_np3_er <- np3_er %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np3_der <- np3_der %>% summarise(avg = mean(score, na.rm = TRUE))
avg_np3_dieser <- np3_dieser %>% summarise(avg = mean(score, na.rm = TRUE))

result_df_np3 <- data.frame(
  NP3 = c("ER", "DER", "DIESER"),
  AVERAGE_RATING = c(avg_np3_er$avg, avg_np3_der$avg, avg_np3_dieser$avg)
)

print(result_df_np1)
print(result_df_np2)
print(result_df_np3)

