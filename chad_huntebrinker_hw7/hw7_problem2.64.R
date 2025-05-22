#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("SENIC_Data.xlsx")

#Length_of_Stay ~ Infection_Risk
#Fit the model
model_5 <- lm(Length_of_Stay~Infection_Risk,data=excel_data)
sum_of_model_5 <- summary(model_5)

#Length_of_Stay ~ Available_facilities_and_services
model_6 <- lm(Length_of_Stay~Available_facilities_and_services,data=excel_data)
sum_of_model_6 <- summary(model_6)

#Length_of_Stay ~ Routine_Chest_X-ray
model_7 <- lm(Length_of_Stay~`Routine_Chest_X-ray`,data=excel_data)
sum_of_model_7 <- summary(model_7)

sum_of_model_5$r.square
sum_of_model_6$r.square
sum_of_model_7$r.square

#The largest reduction in variability with the average length of stay is Infection Risk