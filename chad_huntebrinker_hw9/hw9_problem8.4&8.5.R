#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("Muscle_Mass_Data.xlsx")
excel_data$Age.c <- excel_data$Age-mean(excel_data$Age)

#Muscle_Mass ~ Age
#Fit the model
#Problem 8.4a
model_1 <- lm(Muscle_Mass~Age,data=excel_data)
sum_of_model_1 <- summary(model_1)
plot(Muscle_Mass~Age,data=excel_data)

model_2 <- lm(Muscle_Mass~Age.c + I(Age.c^2),data=excel_data)
sum_of_model_2 <- summary(model_2)
sum_of_model_2
#The quadratic function seems like a good fit.

#Problem 8.4b
summary(model_2)
qf(0.95, 2, 57)
#H0: B1 = B11 = 0, if F_score <= 3.158843
#Ha: not B1 and B11 = 0, if F_score > 3.158843
#F-statistic: 91.84 on 2 and 57 DF
91.84 > 3.158843
#Prove Ha is true.

#Problem 8.4c
predict(model_2, data.frame(Age.c = 48 - mean(excel_data$Age)),
        interval="confidence", se.fit = TRUE, level = 0.95)

#Problem 8.4d
predict(model_2, data.frame(Age.c = 48 - mean(excel_data$Age)),
        interval="prediction", se.fit = TRUE, level = 0.95)

#Problem 8.4e
sum_of_model_2
qt(0.975, 57)
#t* = 1.776
#H0: B11 = 0 if t* <= 2.002465
#Ha: B11 != 0, if t* > 2.002465
1.776 > qt(0.975, 57)
#We can conclude H0 is true

#Problem 8.4f
#Y = 207.350 − 2.96432X + .0148405X^2

#Problem 8.4g
cor(excel_data$Age, excel_data$Age^2)
cor(excel_data$Age.c, excel_data$Age.c^2)
#Yes, the use of a centered variable is helpful here.

#Problem 8.5a
plot(sum_of_model_2$residuals~predict(model_2))

qqnorm(residuals(model_2))
qqline(residuals(model_2))
#Both of these plots seem pretty standard and no need for concern.

#Problem 8.5b
#H0: E{Y} = B0 + B1x + B11x^2, F_score ≤ 1.875188
#HA: E{Y} != B0 + B1x + B11x^2, F_score > 1.875188
reduced_model <- lm(Muscle_Mass~Age.c, data = excel_data)
model_2_residuals <- residuals(model_2)
reduced_model_residuals <- residuals(reduced_model)

#Calculate MSLF
SST <- sum((excel_data$Muscle_Mass - mean(excel_data$Muscle_Mass))^2)

SSR_reduced <- sum((predict(reduced_model) - mean(excel_data$Muscle_Mass))^2)

SSLOF <- SST - SSR_reduced

dfLOF <- length(excel_data$Muscle_Mass) - length(coef(reduced_model)) - 1

MSLF <- SSLOF / dfLOF

#Calculate MSPE

MSPE <- mean((excel_data$Muscle_Mass - predict(model_2))^2)

F_score <- MSLF / MSPE

F_score > qf(0.95, 29, 28)
#Conclude H0

#Problem 8.5c
model_3 <- lm(Muscle_Mass~Age.c + I(Age.c^2) + I(Age.c^3), data = excel_data)
summary(model_3)
#Y = = 82.9273 − 1.26789x + .01504x2 + .000337x3
#t_score = 0.361
#H0: B111 = 0, t_score <= 2.003241
#Ha: B111 != 0, t_score > 2.003241
qt(0.975, 56)
0.361 <= qt(0.975, 56)
#Thus, we can conclude H0.