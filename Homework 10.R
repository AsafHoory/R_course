library(dplyr)
library(pROC)
#read the titanic data
titanic_data <- read.csv("C:/Users/LENOVO/Downloads/Titanic.csv")

#changing the Sex to Gender
titanic_data <- titanic_data |> rename(gender = Sex)
View(titanic_data)

#factor- gender
titanic_data <- titanic_data |> mutate(gender = factor(gender))
contrasts(titanic_data$gender) <- c(1,0)
contrasts(titanic_data$gender)

#factor - department 
dep_factor <- factor(ifelse(titanic_data$PClass == "1st", 0,1))

#descriptive statistics 
titanic_data |>
  group_by(gender, PClass) |>
  summarise(count = n())
titanic_data |> group_by(Survived) |> summarise(count = n())

#logistic reression
model1 <- glm(Survived ~ 1, data = titanic_data, family = binomial)
model2 <- glm(Survived ~ 1 + gender, data = titanic_data, family = binomial)
model3 <- glm(Survived ~ 1 + gender + dep_factor, data = titanic_data, family = binomial)

#Coefficients summary 
summary(model1)
summary(model2)
summary(model3)

#predicted probs
predicted_probs1 <- predict(model1, type = "response")
predicted_probs2 <- predict(model2, type = "response")
predicted_probs3 <- predict(model3, type = "response")

#ROC,AUC 1
ROC1 <- roc(titanic_data$Survived, predicted_probs1)
AUC1 <- auc(ROC1)
plot(ROC1, col = "blue", lwd = 2, main = "ROC model 1")

#ROC,AUC 2
ROC2 <- roc(titanic_data$Survived, predicted_probs2)
AUC2 <- auc(ROC2)
plot(ROC2, col = "blue", lwd = 2, main = "ROC model 2")

#ROC,AUC 3
ROC3 <- roc(titanic_data$Survived, predicted_probs3)
AUC3 <- auc(ROC3)
plot(ROC3, col = "blue", lwd = 2, main = "ROC model 3")


