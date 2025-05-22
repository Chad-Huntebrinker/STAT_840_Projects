#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Crime_Rate_Data.xlsx")

# Fit the model and get the summary of the model
model_1 <- lm(Crime_Rate~Highschool_Diploma_Percentage,data=excel_data)
sum_of_model_1 <- summary(model_1)
sum_of_model_1

# Plot the graph just to see
plot(Crime_Rate~Highschool_Diploma_Percentage,data=excel_data)

#Problem 3.4a - Create stem and leaf plot
stem(excel_data$Highschool_Diploma_Percentage)

#Problem 3.4b - Create boxplot of residuals
residuals_model_1 <- resid(model_1)

boxplot(residuals_model_1, horizontal = TRUE)

#Problem 3.4c - Create a residual plot with the fitted values
fitted_values <- fitted(model_1)
plot(residuals_model_1 ~ fitted_values, main = "Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

#Problem 3.4d - Get the correlation coefficient and the normal probability plot
q <- qqnorm(residuals_model_1, main = "Normal Probability Plot of Residuals")
qqline(residuals_model_1)
cor.test(q$x, q$y, method=c("pearson")) # Table B6 in book

#Problem 3.4e - Do the Brown-Forsythe test
group_1 <- subset(excel_data, excel_data$Highschool_Diploma_Percentage < 69)
group_2 <- subset(excel_data, excel_data$Highschool_Diploma_Percentage >= 69)

model_group1 <- lm(Crime_Rate~Highschool_Diploma_Percentage,data=group_1)
model_group2 <- lm(Crime_Rate~Highschool_Diploma_Percentage,data=group_2)

median_1 <- median(resid(model_group1))
median_2 <- median(resid(model_group2))

abs_dev_group1 <- abs(resid(model_group1) - median_1)
abs_dev_group2 <- abs(resid(model_group2) - median_2)


s <- sqrt((sum((abs_dev_group1 - mean(abs_dev_group1))^2) + sum((abs_dev_group2 - mean(abs_dev_group1))^2)) 
          / (nrow(excel_data) - 2))

two_sample_t <- ((mean(abs_dev_group1) - mean(abs_dev_group2)) / (s * (sqrt(1/nrow(group_1) + 1/nrow(group_2)))))
print(two_sample_t)

#Is H0 true? We find it is not.
abs(two_sample_t) > qt(0.975, 82)
