#Chad Huntebrinker
#Problem 2.27a
#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Muscle_Mass_Data.xlsx")

#Muscle_Mass ~ Age
#Fit the model
model_4 <- lm(Muscle_Mass~Age,data=excel_data)
sum_of_model_4 <- summary(model_4)

#Plot to see the graph
plot(excel_data$Muscle_Mass~excel_data$Age)
abline(model_4,col="green")


#Get b1 and se_b1
b1 <- summary(model_4)$coefficients[2, 1]
se_b1 <- summary(model_4)$coefficients[2, 2]

#Find t score
t_1 <- b1 / se_b1

#If the t_1 > than the t score, than we conclude that H1 is true (which is B1 != 0)
t_1 < qt(0.05, 58)

#Find p score
p_1 <- pt(t_1, 58)

#Problem 2.27c
qt(0.975, 58)
b1
se_b1

#Lower confidence
b1 - (qt(0.975, 58) * se_b1)

#Higher confidence
b1 + (qt(0.975, 58) * se_b1)
