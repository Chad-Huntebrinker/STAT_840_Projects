#Chad Huntebrinker
#Midterm Exam

#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Student_Data.xlsx")

#Fit the model (Problem 13)
model_2 <- lm(GPA~M,data=excel_data)
sum_of_model2 <- summary(model_2)

# Plot the graph just to see
plot(GPA~M,data=excel_data)
abline(model_2,col="blue")

#Problem 14: estimate error variance
sum_of_model2$sigma^2

#Problem 15 and 16:
t.star <- summary(model_2)$coefficients[2,3]
#Test H1 (which we find true so we say there is evidence for linear association)
t.star > qt(0.95, 38)

#Problem 17:
CI <- predict(model_2, se.fit = TRUE, data.frame(M=60), interval = "confidence", level = 0.99)
CI$fit

#Problem 18:
CI_Pio <- predict(model_2, se.fit = TRUE, data.frame(M=99), interval = "confidence", level = 0.99)
CI_Pio$fit