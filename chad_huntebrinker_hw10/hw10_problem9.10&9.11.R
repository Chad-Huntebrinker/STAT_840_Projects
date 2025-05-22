#Chad Huntebrinker

library(leaps)
library(readxl)

excel_data <- read_excel("Job_Proficiency_Data.xlsx")

#Problem 9.10a
stem(excel_data$X1)
stem(excel_data$X2)
stem(excel_data$X3)
stem(excel_data$X4)

#Some noteworthy features include:
#1) All tests have max scores that go over 100, but some max scores are 116 while others
#are 140
#2) The distribution of the data for X2 seem to be more on the larger side (greater than
# 110) while the distribution for X1 are on the less side (less than 110)

#Problem 9.10b
pairs(excel_data)
cor(excel_data)
cor(excel_data[,-1])
#It looks like there might be some correlation concerns with Y for X3 and X4
#There also might be some concerns with X3 and X4

#Problem 9.10c
model_1 <- lm(Y~X1 + X2 + X3 + X4, data=excel_data)
sum_of_model_1 <- summary(model_1)
#Y = -124.38182 + 0.29573X1 + 0.04829X2 + 1.30601X3 + 0.51982X4
sum_of_model_1
sum_of_model_1$adj.r.squared
#the adjusted R^2 has a good score (0.9554702) and the p-value for these predictor variables
#seem okay for all except one.  X2 has a p-value of 0.40383, suggesting that it might be
#better to drop X2 from the model

#Problem 9.11a
ma <- regsubsets(Y~., nbest = 4, data=excel_data)
(sma <- summary(ma))
sma$adjr2
order(sma$adjr2, decreasing = TRUE)[1:4]
#(1) [X1, X3, X4] = 0.9560482 
#(2) [X1, X2, X3, X4] = 0.9554702
#(3) [X1, X3] = 0.9269043 
#(4) [X1, X2, X3] = 0.9246779

#9.11b
#There are a couple of other criteria we could look at to narrow down what model we should use.
#First, we could use some of the other measurements to see if a model is a good fit (like AIC, SSEp,
# and PRESSp) to see if they agree.  Another thing we could do is look at the residuals of these models
#to see if they fit the model well.  Finally, we can look at the simplicity of the model.  In general,
#it's better to have a model that has less predictive variables than one that has more.