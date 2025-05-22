
# Load data in to R
library(readxl)

excel_data <- read_excel("Heights of Mothers and Daughters.xlsx")

# daughter ~ mother
# Fit the model
model_5 <- lm(daughter~mother,data=excel_data)
sum_of_data <- summary(model_5)
e <- residuals(model_5)
sum(e)

# Plot to make sure a linear relationship makes sense
plot(excel_data$daughter~excel_data$mother)
abline(model_5,col="green")


#Get b1 and se_b1
b1 <- summary(model_5)$coefficients[2, 1]
se_b1 <- summary(model_5)$coefficients[2, 2]


MSE <- summary(model_5)$sigma^2
#Find t score
qt(0.95, 1373)
t_1 <- b1 / se_b1
t_1 > qt(0.95, 1373)

#Find p score
p_1 <- pt(t_1, 1373)

#Problem 2.27c
qt(0.975, 1373)
b1
se_b1
b1 - (qt(0.975, 1373) * se_b1)
b1 + (qt(0.975, 1373) * se_b1)

sum_of_data
sum_of_data$coefficients[2,4]

#Mean of problem 2
mean(excel_data$mother)
t.test(excel_data$daughter, conf.level = 0.95)
