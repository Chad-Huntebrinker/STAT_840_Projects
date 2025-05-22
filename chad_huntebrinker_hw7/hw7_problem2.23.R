#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Grade_Point_Average_Data.xlsx")

# GPA ~ ACT
# Fit the model and get the summary of the model
model_1 <- lm(GPA~ACT_Score,data=excel_data)
sum_of_model_1 <- summary(model_1)
sum_of_model_1

# Plot the graph just to see
plot(GPA~ACT_Score,data=excel_data)

#Problem 2.23a
anova_table <- anova(model_1)

#Problem 2.23b
#MSR
anova_table$`Mean Sq`[1]

#MSE
anova_table$`Mean Sq`[2]

#They will estimate the same quantity when the null hypothesis is true (H0: B1 = 0).

#Problem 2.23c
#H0: B1 = 0: F* <= qf(1 - 0.01, 1, 118)
#Ha: B1 != 0: F* > qf(1 - 0.01, 1, 118)
#Check to see if Ha is true:
anova_table$`F value`[1]
qf(1 - 0.01, 1, 118)
anova_table$`F value`[1] > qf(1 - 0.01, 1, 118)
#Conclusion: B1 != 0, there is a linear association

#Problem2.23d
model_1_reduced <- lm(GPA~1,data=excel_data)
anova_table2 <- anova(model_1_reduced,model_1)

#Absolute magnitude of reduction
anova_table2$`Sum of Sq`[2]
#Relative reduction
anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1]
#It's also known as R^2 or coefficient of determination
summary(model_1)$r.square

#Problem 2.23e
sqrt(anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1])
#r = +0.2694818

#Problem 2.23f
#In this case, r has a more clear-cut operational interpration.  This is due to the answer
#of r (+0.2694818) showing that there is a slight positive linear relationship between
#the two variables. And we can see that is the case on the graph too (positive linear relationship
#between points until we get to around 27).