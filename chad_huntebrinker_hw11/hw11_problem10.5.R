#Chad Huntebrinker

library(readxl)
require(faraway)

excel_data <- read_excel("Brand_preference_data.xlsx")

#Problem 10.5a
model_1 <- lm(Yi~Xi1+Xi2, data = excel_data)

prplot(model_1,1)

prplot(model_1,2)

#Problem 10.5b
#It looks like both Xi1 and Xi2 would contribute to the model due to both of them having
#a linear relationship with the residuals of the model

#Problem 10.5c
model_Yi_X1 <- lm(Yi ~ Xi1, data = excel_data)
model_X2_X1 <- lm(Xi2 ~ Xi1, data = excel_data)


model_2 <- lm(model_Yi_X1$residuals~model_X2_X1$residuals)

summary(model_2)