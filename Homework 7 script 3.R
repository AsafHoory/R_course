load("df.rdata")
library(ggplot2)

#mean and sd of rt in congruent trials
df_congrouent_only <- df |> 
  filter(congruency == "congruent") |> 
  summarise(mean_rt_congrouent = mean(rt, na.rm = TRUE), sd_rt_congruent = sd(rt, na.rm = TRUE))

#mean and sd of rt in incongruent trials
df_incongrouent_only <- df |>
  filter(congruency == "incongruent") |>
  summarise(mean_rt_incongruent = mean(rt, na.rm = TRUE), sd_rt_incongruent = sd(rt, na.rm = TRUE))
print(df_incongrouent_only)

#T test of RT between cong and incong
t_test_result <- t.test(
  rt ~ congruency,
  data = df,
)
print(t_test_result)

#Discriptive plot for for RT between conditions 
View(df)
#average and sd by condition
summary_discriptive_stat <- df |>
  group_by(congruency) |>
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE)
  )
print(summary_discriptive_stat)
 
ggplot(data = summary_discriptive_stat, aes(x = congruency, y = mean_rt)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_rt - sd_rt, ymax = mean_rt + sd_rt)) +
  labs(
    title = "descriptive plot for both conditions",
    x = "Congruency",
    y = "Mean RT"
  )





