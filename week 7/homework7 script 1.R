# R course for beginners
# Week 7
# Asaf Hoory 211671714

rm(list = ls())
library(dplyr)
files_names = dir("./data.stroop") #creating a vector with the names of the files
#creating one data frame
df <- data.frame()
for (file in files_names) {
  df <- rbind(df, read.csv(paste0("./data.stroop/", file)))
}
View(df)
#adding task, congruency, accuracy, and deleting non-relevant columns 
df <- df |>
  mutate(
    task = ifelse(grepl('word', condition), 'word_naming', 'color_naming'),
    congruency  = ifelse(grepl('incong', condition), 'incongruent', 'congruent'),
    accuracy = ifelse(participant_response == correct_response, 1, 0)
  )


df <- df |> select(subject, block, trial, task, congruency, accuracy, rt)

df <- df |> mutate(
  subject = as.factor(subject),
  task = as.factor(task),
  congruency = as.factor(congruency),
  block = as.numeric(block),
  trial = as.numeric(trial),
  accuracy = as.numeric(accuracy),
  rt = as.numeric(rt)
)

contrasts(df$task)
contrasts(df$congruency)


save(df, file = "df.rdata")


