#Chad Huntebrinker

library(readxl)
library(lawstat)

excel_data <- read_excel("Commercial_properties_data.xlsx")

#Problem 6.18a
stem(excel_data$Xi1)
stem(excel_data$Xi2)
stem(excel_data$Xi3)
stem(excel_data$Xi4)
#These plots provide the range of values that these varaiables are (which we can use
#to see if there are any outliers or see if it's distributed well).

#Problem 6.18b
pairs(Yi~Xi1+Xi2+Xi3+Xi4,data=excel_data) 
cor(excel_data)
#There seems to be a bit of a linear relationship between Yi and Xi4. Besides that,
#there really isn't anything

#Problem 6.18c
model_2 <- lm(Yi~Xi1+Xi2+Xi3+Xi4, data = excel_data)
sum_of_model_2 <- summary(model_2)
sum_of_model_2
#Yi = 1.220e+01 - 1.420e-01Xi1 + 2.820e-01Xi2 + 6.193e-01Xi3 + 7.924e-06Xi4

#Problem 6.18d
sum_of_model_2$residuals
boxplot(sum_of_model_2$residuals)
#While it seems to be fairly symmetrical, we see that there are some outlier
#residuals in the plot

#Problem 6.18e
plot(model_2$residuals~predict(model_2),pch=16,xlab = "Yhat",ylab="Residuals", data = excel_data)
plot(model_2$residuals~Xi1,pch=16,ylab="Residuals", data = excel_data)
plot(model_2$residuals~Xi2,pch=16,ylab="Residuals", data = excel_data)
plot(model_2$residuals~Xi3,pch=16,ylab="Residuals", data = excel_data)
plot(model_2$residuals~Xi4,pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi1 * excel_data$Xi2
plot(model_2$residuals~temp_values, xlab = "Xi1 * Xi2", pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi1 * excel_data$Xi3
plot(model_2$residuals~temp_values, xlab = "Xi1 * Xi3", pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi1 * excel_data$Xi4
plot(model_2$residuals~temp_values, xlab = "Xi1 * Xi4", pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi2 * excel_data$Xi3
plot(model_2$residuals~temp_values, xlab = "Xi2 * Xi3", pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi2 * excel_data$Xi4
plot(model_2$residuals~temp_values, xlab = "Xi2 * Xi4", pch=16,ylab="Residuals", data = excel_data)
temp_values <- excel_data$Xi3 * excel_data$Xi4
plot(model_2$residuals~temp_values, xlab = "Xi3 * Xi4", pch=16,ylab="Residuals", data = excel_data)

qqnorm(residuals(model_2))
qqline(residuals(model_2))
#Yhat: relatively normal
#Xi1:relatively normal, but there's a lack of values between 5 and 10
#Xi2: relatively normal
#Xi3: relatively normal, but there's some outliers at 0.6
#Xi4: relatively normal
#Xi1 * Xi2: relatively normal, but lack of values between 50 and 100
#Xi1 * Xi3: relatively normal, but outliers beyond 1
#Xi1 * Xi4: relatively normal
#Xi2 * Xi3: relatively normal, but there's some outliers past 2
#Xi2 * Xi4: relatively normal
#Xi3 * Xi4: relatively normal, but there's some outliers past 50,000
#Normal plot: relatively normal

#Problem 6.18f
#Yes, you could.

#Problem 6.18g
#H0: t*bf <= qt(0.975,79), the error variance is constant
#Ha: t*bf > qt(0.975,79), the error variance is not constant
mean(model_2$fitted.values)
sum(model_2$fitted.values < mean(model_2$fitted.values))

group_1_values <- model_2$fitted.values[model_2$fitted.values < mean(model_2$fitted.values)]
group_2_values <- model_2$fitted.values[model_2$fitted.values > mean(model_2$fitted.values)]

fitted_values <- c(group_1_values, group_2_values)
groups <- factor(rep(c("Group1", "Group2"), times = c(40, 41)))

levene.test(fitted_values, groups)
#Test if the H0 is true
1.6323 <= qt(0.975,79)
#It is true, so the error variance is constant

#Problem 6.19a
#H0: B1 = B2 = 0
#Ha: not all Bk = 0 (k = 1 and 2)
sum_of_model_2
#F score = 26.76 on 4 and 76 DF
26.76 > qf(1 - 0.05, 4, 76)
#Ha is true.  Meaning that B1 and B2 and B3 and B4 all do not equal 0
#p-value: 7.272e-14

#Problem 6.19b
confint(model_2, level=0.9875)

#Problem 6.19c
sum_of_model_2$r.squared
#This shows there seems to be a decent amount of linear association between
#Xi1, Xi2, Xi3, and Xi4 combined and Yi

#Problem 6.20
sum_of_model_2
excel_data2 <- read_excel("Mean_rental_rates_data.xlsx")

value1 <- predict(model_2, data.frame(Xi1 = excel_data2$Xi1[1], Xi2 = excel_data2$Xi2[1],
                                      Xi3 = excel_data2$Xi3[1], Xi4 = excel_data2$Xi4[1]),
                                      interval = "confidence", se.fit = TRUE)

value2 <- predict(model_2, data.frame(Xi1 = excel_data2$Xi1[2], Xi2 = excel_data2$Xi2[2],
                                      Xi3 = excel_data2$Xi3[2], Xi4 = excel_data2$Xi4[2]),
                                      interval = "confidence", se.fit = TRUE)

value3 <- predict(model_2, data.frame(Xi1 = excel_data2$Xi1[4], Xi2 = excel_data2$Xi2[3],
                                      Xi3 = excel_data2$Xi3[4], Xi4 = excel_data2$Xi4[3]),
                                      interval = "confidence", se.fit = TRUE)

value4 <- predict(model_2, data.frame(Xi1 = excel_data2$Xi1[4], Xi2 = excel_data2$Xi2[4],
                                      Xi3 = excel_data2$Xi3[4], Xi4 = excel_data2$Xi4[4]), 
                                      interval = "confidence", se.fit = TRUE)
value1$fit
value2$fit
value3$fit
value4$fit

#Problem 6.21
excel_data3 <- read_excel("No_rental_information_data.xlsx")

value1 <- predict(model_2, data.frame(Xi1 = excel_data3$Xi1[1], Xi2 = excel_data3$Xi2[1],
                                      Xi3 = excel_data3$Xi3[1], Xi4 = excel_data3$Xi4[1]),
                  interval="prediction", se.fit = TRUE)

value2 <- predict(model_2, data.frame(Xi1 = excel_data3$Xi1[2], Xi2 = excel_data3$Xi2[2],
                                      Xi3 = excel_data3$Xi3[2], Xi4 = excel_data3$Xi4[2]),
                  interval="prediction", se.fit = TRUE)

value3 <- predict(model_2, data.frame(Xi1 = excel_data3$Xi1[3], Xi2 = excel_data3$Xi2[3],
                                      Xi3 = excel_data3$Xi3[3], Xi4 = excel_data3$Xi4[3]),
                  interval="prediction", se.fit = TRUE)
value1$fit
value2$fit
value3$fit
