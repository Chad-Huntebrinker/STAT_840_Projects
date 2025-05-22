#Chad Huntebrinker
#Problem 2.4b
#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Grade_Point_Average_Data.xlsx")

# GPA ~ ACT
# Fit the model and get the summary of the model
model_1 <- lm(GPA~ACT_Score,data=excel_data)
sum_of_model_1 <- summary(model_1)
sum_of_model_1
sum_of_model_1$coefficients[2,4]

# Plot the graph just to see
plot(GPA~ACT_Score,data=excel_data)
abline(model_1,col="red")

#Calculate the t score
b1 <- sum_of_model_1$coefficients[2, 1]
se_b1 <- sum_of_model_1$coefficients[2, 2]

#a = 0.01
t_1 <- b1 / se_b1

#If the t_1 > than the t score, than we conclude that H1 is true (which is B1 != 0)
t_1 > qt(0.995, 118)

#Problem 2.4c
#Calculate the p_value
p_1 <- 2 * (1 - pt(t_1, 118))

#If the p score is less than alpha, than H1 is true (which is B1 != 0)
p_1 < 0.01