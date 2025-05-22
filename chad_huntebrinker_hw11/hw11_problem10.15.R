#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Brand_preference_data.xlsx")
model_1 <- lm(Yi~Xi1+Xi2, data = excel_data)

#Problem 10.15a
pairs(excel_data)
cor(excel_data[,-1])
#It shows there is no association between X1 and X2

#Problem 10.15b
vif(model_1)

#They are both equal to 1 because there is no correlation between the predictor variables