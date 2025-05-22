#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Brand_preference_data.xlsx")

#Problem 6.5a
pairs(Yi~Xi1+Xi2,data=excel_data) 
cor(excel_data)
#We see that there is a strong linear correlation between Yi and Xi1, but not so much
#anywhere else.

#Problem 6.5b
model_1 <- lm(Yi~Xi1+Xi2, data = excel_data)
sum_of_model_1 <- summary(model_1)
sum_of_model_1
#Yi1 = 37.65 + 4.425 * Xi1 + 4.375 * Xi2

#Problem 6.5c
sum_of_model_1$residuals
boxplot(sum_of_model_1$residuals)
#We see the residuals are fairly well distrubted.

#Problem 6.5d
# Plot residuals versus each X to examine the relationship
plot(model_1$residuals~predict(model_1),pch=16,ylab="Residuals", data = excel_data)
plot(model_1$residuals~Xi1,pch=16,ylab="Residuals", data = excel_data)
plot(model_1$residuals~Xi2,pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi1 * excel_data$Xi2
plot(model_1$residuals~temp_values,pch=16,ylab="Residuals", data = excel_data)

qqnorm(residuals(model_1))
qqline(residuals(model_1))
#What we find is:
#Yhat seems to have more of a curve.
#Xi1 seems to be distributed well, but only at values 4, 6, 8, and 10
#Xi2 is the same but only at values 2 and 4
#Xi1 * Xi2 seems to be distributed well
#The normal plot seems reasonable

#Problem 6.6a
#H0: B1 = B2 = 0
#Ha: not all Bk = 0 (k = 1 and 2)
sum_of_model_1
#F score = 129.1 on 2 and 13 DF
129.1 > qf(1 - 0.01, 2, 13)
#Ha is true.  Meaning that both B1 and B2 != 0

#Problem 6.6b
#P-value = 2.658e-09 (which is basically 0+)

#Problem 6.6c
confint(model_1, level=0.995)