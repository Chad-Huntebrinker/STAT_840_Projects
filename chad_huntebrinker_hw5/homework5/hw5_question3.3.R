#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Grade_Point_Average_Data.xlsx")

# GPA ~ ACT
# Fit the model and get the summary of the model
model_1 <- lm(GPA~ACT_Score,data=excel_data)
sum_of_model_1 <- summary(model_1)
sum_of_model_1

# Plot the graph just to see
plot(GPA~ACT_Score,data=excel_data)

#Problem 3.3a - Create boxplot
boxplot(excel_data$ACT_Score, horizontal = TRUE)

#Problem 3.3b - Create dot plot of residuals
residuals_model_1 <- resid(model_1)

stripchart(residuals_model_1, main = "Dot Plot of Residuals", xlab = "Residuals", method = "jitter")


#Problem 3.3c - Create plot of residuals against the fitted values
fitted_values <- fitted(model_1)
plot(residuals_model_1 ~ fitted_values, main = "Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

#Problem 3.3d - Create Normal Plot
q <- qqnorm(residuals_model_1, main = "Normal Probability Plot of Residuals")
qqline(residuals_model_1)
cor.test(q$x, q$y, method=c("pearson")) # Table B6 in book

#Problem 3.3e - calculate t sample test statistic and the Brown-Forsythe test
group_1 <- subset(excel_data, excel_data$ACT_Score < 26)
group_2 <- subset(excel_data, excel_data$ACT_Score >= 26)

model_group1 <- lm(GPA~ACT_Score,data=group_1)
model_group2 <- lm(GPA~ACT_Score,data=group_2)

median_1 <- median(resid(model_group1))
median_2 <- median(resid(model_group2))

abs_dev_group1 <- abs(resid(model_group1) - median_1)
abs_dev_group2 <- abs(resid(model_group2) - median_2)


s <- sqrt((sum((abs_dev_group1 - mean(abs_dev_group1))^2) + sum((abs_dev_group2 - mean(abs_dev_group1))^2)) 
          / (nrow(excel_data) - 2))

two_sample_t <- ((mean(abs_dev_group1) - mean(abs_dev_group2)) / (s * (sqrt(1/nrow(group_1) + 1/nrow(group_2)))))
print(two_sample_t)

#Is H0 true? We find it is not
abs(two_sample_t) > qt(0.995, 118)

#Problem 3.3f - Plot x2 and x3 with the residuals
plot(residuals_model_1 ~ excel_data$Intelligence_Test_Score, 
     main = "X2 Residual Plot", xlab = "Intelligence Test Score", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(residuals_model_1 ~ excel_data$Class_Rank_Percentile, 
     main = "X3 Residual Plot", xlab = "Class Rank Percentile", ylab = "Residuals")
abline(h = 0, lty = 2)
