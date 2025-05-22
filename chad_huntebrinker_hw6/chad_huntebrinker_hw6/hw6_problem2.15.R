#Chad Huntebrinker

#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Airfreight_Breakage_Data.xlsx")

#Number_Broker ~ Number_Transferred
#Fit the model
model_4 <- lm(Number_Broker~Number_Transferred,data=excel_data)
sum_of_model_4 <- summary(model_4)

#Plot to see the graph
plot(Number_Broker~Number_Transferred, data = excel_data)
abline(model_4)

#Problem 2.15a
#Getting the mean breakage at X = 2 and 4.  Also getting a 99% confidence interval at those values too.
predict(model_4, data.frame(Number_Transferred=2),interval="confidence",level=0.99)

predict(model_4, data.frame(Number_Transferred=4),interval="confidence",level=0.99)

#Problem 2.15d
#Getting the boundary values of the 99 percent confidence band for the regression line at X = 2 and 4.
Y_hat2 <- predict(model_4, data.frame(Number_Transferred=2))
Y_hat4 <- predict(model_4, data.frame(Number_Transferred=4))

W <- sqrt(2*qf(1-0.01,2,10-2))

CI2 <- predict(model_4, se.fit = TRUE, data.frame(Number_Transferred=2), interval = "confidence", level = 0.99)
lower_bound2 <- Y_hat2 - W * CI2$se.fit
upper_bound2 <- Y_hat2 + W * CI2$se.fit

CI4 <- predict(model_4, se.fit = TRUE, data.frame(Number_Transferred=4), interval = "confidence", level = 0.99)
lower_bound4 <- Y_hat4 - W * CI4$se.fit
upper_bound4 <- Y_hat4 + W * CI4$se.fit

#We find that both of these confidence bands are wider than the confidence
#interval in part a and we would expect that.