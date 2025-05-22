#Chad Huntebrinker

library(readxl)
library(dplyr)
require(faraway)

excel_data <- read_excel("Job_Proficiency_Data.xlsx")

#Problem 10.19a
model_1 <- lm(Y~X1 + X3, data=excel_data)
model_1_residuals <- model_1$residuals
excel_data <-excel_data %>% 
  mutate(X1X3 = X1*X3)

plot(model_1_residuals~predict(model_1))
plot(model_1_residuals~X1, data = excel_data)
plot(model_1_residuals~X2, data = excel_data)
plot(model_1_residuals~X3, data = excel_data)
plot(model_1_residuals~X4, data = excel_data)
plot(model_1_residuals~X1X3, data = excel_data)

#The Yhat graph seems okay along with X1, X2, and X3 and X1X3.
#X4 might have a curve (starts up, goes down, then starts going up again).
#Overall, doesn't look like there's any modification needed.

#Problem 10.19b
model_X1_X3 <- lm(X1 ~ X3, data = excel_data)
model_X3_X1 <- lm(X3 ~ X1, data = excel_data)

prplot(model_X1_X3, 1)
prplot(model_X3_X1, 1)

#No modification seems to be needed.

#Problem 10.19c
qqnorm(model_1$residuals)
qqline(model_1$residuals)
cor(model_1$residuals, qqnorm(model_1$residuals, plot.it = FALSE)$x)
#The table has it listed between 0.957 and 0.96 and the correlation coefficient is 0.98
#Thus, it is reasonable.

#Problem 10.19d
#Ha: If SDR > 2.508325, then it is an outlier
#H1: If SDR <= 2.508325, then it is not an outlier

model_1_SDR <- rstudent(model_1)
bon_cutoff <- qt(1 - 0.5 / 50, df = model_1$df.residual)
outliers <- abs(model_1_SDR) > bon_cutoff

#The following are outliers
which(outliers)
model_1_SDR[16]

#Problem 10.19e
which(hatvalues(model_1)>2*3/25)
excel_data[c(7,18),]

#Problem 10.19f
dffits_values <- dffits(model_1)
dfbetas_values <- dfbetas(model_1)
cooks_values <- cooks.distance(model_1)

dffits_values[c(7, 16, 18)]
#Case 7 and 16 are okay, case 18 indicates a possible influence as it is almost equal to 1

dfbetas_values[c(7, 16, 18)]
#All the cases seem okay (they are less than 1)

cooks_values[c(7, 16, 18)]
#Case 7 and 16 are okay, case 18 indicates a possible influence as it is greater than
#the 10 or 20 percent cutoff.

#As a result, case 7 and 16 do not seem to be outliers.  Case 18 indicates some suspicion and
#should be investigated deeper.

#Problem 10.19g
vif_values <- vif(model_1)
vif_values
#They indicate a basically no multicollinearity between the predictor variables.