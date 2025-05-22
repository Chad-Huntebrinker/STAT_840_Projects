#Chad Huntebrinker

library(leaps)
library(readxl)

excel_data <- read_excel("SENIC_Data.xlsx")
data_part1 <- excel_data[1:56,]
data_part2 <- excel_data[57:113,]

#Problem 9.27a
#The best subset according to the Cp criterion is using age, check x-ray ratio, and census.
#So we will use that as our regression model
model_1 <- lm(log(Length_of_Stay)~Age + `Routine_Chest_X-ray` + Average_daily_census, data = data_part2)

validation_model <- lm(log(Length_of_Stay)~Age + `Routine_Chest_X-ray` + Average_daily_census, data = data_part1)

summary(model_1)$coefficients[, "Std. Error"]
sum(model_1$residuals^2)
anova(model_1)["Residuals", "Mean Sq"]

summary(validation_model)$coefficients[, "Std. Error"]
sum(validation_model$residuals^2)
anova(validation_model)["Residuals", "Mean Sq"]

#The validation model seems similar to the model-building one.

#Problem 9.27b
model_1_MSE <- mean((data_part2$Length_of_Stay - predict(model_1))^2)
validation_model_MSPE <- mean((data_part1$Length_of_Stay - predict(validation_model))^2)

model_1_MSE
validation_model_MSPE
#The MSE and MSPE have a slight difference which wouldn't indicate any bias

#Problem 9.27c
model_2 <- lm(log(Length_of_Stay)~Age + `Routine_Chest_X-ray` + Average_daily_census, data=excel_data)
summary(model_2)$coefficients

summary(model_1)$coefficients

#The coefficients and standard deviations are different from those of the model building set.
#We would expect there to be a difference in the estimates due to new examples being introduced.