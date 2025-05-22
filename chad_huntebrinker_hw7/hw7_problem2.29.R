#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("Muscle_Mass_Data.xlsx")

#Muscle_Mass ~ Age
#Fit the model
model_3 <- lm(Muscle_Mass~Age,data=excel_data)
sum_of_model_3 <- summary(model_3)

#Plot to see the graph
plot(Muscle_Mass~Age, data = excel_data)

#Problem 2.29a
SSE_model_3 <- excel_data$Muscle_Mass - predict(model_3, data.frame(Age=excel_data$Age))


SSR_model_3 <- predict(model_3, data.frame(Age=excel_data$Age)) - mean(excel_data$Muscle_Mass)

plot(SSE_model_3 ~ Age, data = excel_data)
plot(SSR_model_3 ~ Age, data = excel_data)

#It looks like the SSR plays a larger component of SSTO.
#That means that R^2 will be closer to 1, leading to a greater degree in linearity


#Problem 2.29b
anova_table <- anova(model_3)

#Problem 2.29c
#H0: B1 = 0: F* <= qf(1 - 0.05, 1, 58)
#Ha: B1 != 0: F* > qf(1 - 0.05, 1, 58)
#Check to see if Ha is true:
anova_table$`F value`[1]
qf(1 - 0.05, 1, 58)
anova_table$`F value`[1] > qf(1 - 0.05, 1, 58)
#Conclusion: B1 != 0, there is a linear association

#Problem 2.29d
model_3_reduced <- lm(Muscle_Mass~1,data=excel_data)
anova_table2 <- anova(model_3_reduced,model_3)

1 - anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1]
#0.2499332 or 24.99%
#It is relatively small

#Problem 2.29e
#R^2 = 0.7500668
anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1]

#r = -0.866064
sqrt(anova_table2$`Sum of Sq`[2] / anova_table2$RSS[1])
