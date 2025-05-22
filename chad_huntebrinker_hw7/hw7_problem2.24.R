#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Copier_Maintenance_Data.xlsx")

# GPA ~ ACT
# Fit the model and get the summary of the model
model_2 <- lm(Total_Minutes~Number_Of_Copiers,data=excel_data)
sum_of_model_2 <- summary(model_2)
sum_of_model_2

# Plot the graph just to see
plot(Total_Minutes~Number_Of_Copiers,data=excel_data)

#Problem 2.24a
anova_table <- anova(model_2)

#Correction of mean
45 * mean(excel_data$Total_Minutes)^2

#SSTOU
sum(excel_data$Total_Minutes^2)

#Problem 2.24b
#H0: B1 = 0: F* <= qf(1 - 0.01, 1, 43)
#Ha: B1 != 0: F* > qf(1 - 0.01, 1, 43)
#Check to see if Ha is true:
anova_table$`F value`[1]
qf(1 - 0.1, 1, 43)
anova_table$`F value`[1] > qf(1 - 0.1, 1, 43)
#Conclusion: Ha = True, B1 != 0, there is a linear association

#Problem 2.24c
model_2_reduced <- lm(Total_Minutes~1,data=excel_data)
anova_table2 <- anova(model_2_reduced,model_2)

#Relative reduction
anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1]
#It's also known as R^2 or coefficient of determination

#Problem 2.24d
sqrt(anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1])
#r = +0.978517

#Problem 2.24e
#In this case, R^2.  The reason why is
