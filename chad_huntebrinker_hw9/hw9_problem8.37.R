#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("CDI_Data.xlsx")

#Problem 8.37
excel_data$Pop_Density <- excel_data$Total_Pop / excel_data$Land_Area
Crime_Rate <- excel_data$Serious_Crimes / excel_data$Total_Pop
Pop_Density.c <- excel_data$Pop_Density - mean(excel_data$Pop_Density)
Unemployment.c <- excel_data$Unemployment - mean(excel_data$Unemployment)

model_1 <- lm(Crime_Rate~Pop_Density.c + Unemployment.c + I(Pop_Density.c^2) +
                I(Unemployment.c^2) + Pop_Density.c*Unemployment.c)
sum_of_model_1 <- summary(model_1)

plot(model_1$residuals~model_1$fitted.values)

sum_of_model_1
#Multiple R-squared:  0.2485,	Adjusted R-squared:  0.2398
#The second-order model appears to fit the data pretty well, although there are some outliers
#in the regression function

#Problem 8.37b
#H0: p_value > 0.01, drop the quadratic and interaction terms
#Ha: p_value < 0.01, keep the quadratic and interaction terms
reduced_model <- lm(Crime_Rate~Pop_Density.c + Unemployment.c)
anova(reduced_model, model_1)
#p_value = 0.02278 > 0.01
0.02278 > 0.01
#H0 is true, so drop those terms

#Problem 8.37c
#Problem doesn't specify whether these variables need to be centered or not.
#So, I will do both.
model_2 <- lm(Crime_Rate~Total_Pop + Land_Area + Unemployment + I(Total_Pop^2),
              data = excel_data)
summary(model_2)
#Multiple R-squared:  0.1444,	Adjusted R-squared:  0.1365

Total_Pop.c <- excel_data$Total_Pop - mean(excel_data$Total_Pop)
Land_Area.c <- excel_data$Land_Area - mean(excel_data$Land_Area)

model_3 <- lm(Crime_Rate~Total_Pop.c + Land_Area.c + Unemployment.c + I(Total_Pop.c^2))
summary(model_3)
#Multiple R-squared:  0.1444,	Adjusted R-squared:  0.1365

#While the coefficients are different, they both are on the lower end of the spectrum.