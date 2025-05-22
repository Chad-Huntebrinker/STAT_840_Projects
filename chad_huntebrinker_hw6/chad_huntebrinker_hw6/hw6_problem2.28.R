#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("Muscle_Mass_Data.xlsx")

#Muscle_Mass ~ Age
#Fit the model
model_5 <- lm(Muscle_Mass~Age,data=excel_data)
sum_of_model_5 <- summary(model_5)

#Plot to see the graph
plot(Muscle_Mass~Age, data = excel_data)
abline(model_5)

#Problem 2.28a
#Get a 95% confidence interval for the mean of the muscle mass (Y) when X = 60
predict(model_5, data.frame(Age=60),interval="confidence",level=0.95)

#We find the mean is 84.94683 with an upper bound of 87.05895 and lower bound of 82.83471
#This means that with 95% confidence we can say that the mean muscle mass is between 82.83 and 87.05

#Problem 2.28b
#Get a 95% prediction interval for the muscle mass when X = 60
predict(model_5, data.frame(Age=60),interval="prediction",level=0.95)

#With the mean being 84.94683, we find the lower bound is 68.45067 and the upper bound is 101.443
#This seems to indicate the prediction interval is relatively precise

#Problem 2.28c
#Get the boundary values of the 95% confidence band for the regression line at Xh = 60
CI <- predict(model_5, se.fit = TRUE, data.frame(Age=60), interval = "confidence", level = 0.95)

W <- sqrt(2*qf(1-0.05, 2, 60-2))
lower_bound <- CI$fit[1] - W * CI$se.fit
upper_bound <- CI$fit[1] + W * CI$se.fit

#We find the confidence band is wider than the confidence interval in part a.
#Yes, we would expect that.