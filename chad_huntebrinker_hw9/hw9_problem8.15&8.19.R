#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("Copier_Maintenance_Data.xlsx")

#Problem 8.15a & b
model_1 <- lm(Total_Minutes~Number_Of_Copiers + Type_Of_Copier,data=excel_data)
summary(model_1)

#B0 is the intercept
#B1 is one of the slopes, specifically related with the number of copiers
#B2 is the other slopes, specifically related to the copier size.
#Y = -0.9225 + 15.0461X1 + 0.7587X2

#Problem 8.15c
mean(excel_data$Number_Of_Copiers)
predict(model_1, data.frame(Number_Of_Copiers = 5.111111, Type_Of_Copier = 0),
        interval="confidence", se.fit = TRUE, level = 0.95)
predict(model_1, data.frame(Number_Of_Copiers = 5.111111, Type_Of_Copier = 1),
        interval="confidence", se.fit = TRUE, level = 0.95)

#Problem 8.15d
#We would be interested in X1 because the service time is still impacted by the
#number of copiers even if we are interested in the effect of the type of copier (2 smaller
#copiers might have a similiar amount of service time as 1 larger one, for example).

#Problem 8.15e
temp_values <- excel_data$Number_Of_Copiers * excel_data$Type_Of_Copier
plot(Total_Minutes~temp_values, data = excel_data)
#Yes, there seems to be a pattern so a interaction term would be helpful.

#Problem 8.19a
model_2 <- lm(Total_Minutes~Number_Of_Copiers + Type_Of_Copier +
                Number_Of_Copiers*Type_Of_Copier, data = excel_data)
summary(model_2)
#Y = 2.8131 + 14.3394X1 - 8.1412X2 + 1.7774X1X2

#Problem 8.19b
#H0: B3 = 0, t_value <= qt(0.95, 41)
#HA: B3 != 0, t_value > qt(0.95, 41)
summary(model_2)
#t_value = 1.824
#p_value = 0.0755

qt(0.95, 41)

#Test H0
1.824 <= qt(0.95, 41)
#Reject H0, B3 != 0, keep it in the model.