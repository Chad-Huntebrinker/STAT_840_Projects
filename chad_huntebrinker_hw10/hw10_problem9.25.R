#Chad Huntebrinker

library(leaps)
library(readxl)

excel_data <- read_excel("SENIC_Data.xlsx")
#Use only rows 57 thru 113 and remove Region and Medical Affiliation
model_data <- excel_data[57:113, ]
model_data <- model_data[, -9]
model_data <- model_data[, -8]
#Note: I believe we should also remove the ID number from the data we use as this shouldn't have
#an impact on the patient's length of stay.  But we'll keep it in as the instructions didn't say
#to remove it.

#Problem 9.25a
for (col_name in names(model_data)) {
  dotchart(model_data[[col_name]], main = paste("Dot Plot of", col_name), xlab = col_name)
}
#Variables that have outliers:
#Number of Beds
#Average daily census
#Number of nurses

#Variables that have spaces in the data:
#Available facilities and services

#Also, ID number is completely linear as patients are assigned them as they are admitted

#Problem 9.25b
pairs(model_data)
cor(model_data)
#Remove ID number and length of stay
cor(model_data[,-2])
#Major concern with linear pairwise associations:
#number of beds and average daily census = 0.99000302
#average daily census and number of nurses 0.90388584
#number of beds and number of nurses = 0.90892888

#Minor concern:
#available facilities and services and number of nurses = 0.7070559
#available facilities and services and number of beds = 0.7644784
#average daily census and available facilities and services = 0.7294165

#Problem 9.25c
model_subsets <- regsubsets(log(Length_of_Stay)~., nbest = 12, data=model_data)
(sum_of_model_subsets <- summary(model_subsets))
sum_of_model_subsets$cp
order(sum_of_model_subsets$cp, decreasing = FALSE)[1:3]
sum_of_model_subsets$which[34,]
sum_of_model_subsets$which[46,]
sum_of_model_subsets$which[47,]
#(1) 4.032107
#(Intercept), ID_Number, Age, Routine_Chest_X-ray, Average_daily_census
5 - 4.032107
#(2) 4.414536
#(Intercept), ID_Number, Age, Routine_Chest_X-ray, Number_of_Beds, Average_daily_census
6 - 4.414536
#(3) 4.951662
#(Intercept), ID_Number, Age, Routine_Culturing_Ratio, Routine_Chest_X-ray, Average_daily_census
6 - 4.951662

#The first model (the one with with the predictor values of Intercept, ID_Number, 
#Age, Routine_Chest_X-ray, Average_daily_census) has the least bias as the Cp score is closest to the
#number of predictor variables it uses.