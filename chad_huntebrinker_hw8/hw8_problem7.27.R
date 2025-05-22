#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Commercial_properties_data.xlsx")

#Problem 7.27a
model_3 <- lm(Yi~Xi1+Xi4, data = excel_data)
sum_of_model_3 <- summary(model_3)
sum_of_model_3

#Yi = -1.145e-01 * Xi1 + 1.045e-05 * Xi4

#Problem 7.27b
#The coefficients changed a little bit, 
#Xi1 increased positively a little and Xi4 got a little bigger.

#Problem 7.27c
model4 <- lm(Yi~Xi3, data = excel_data)
model5 <- lm(Yi~Xi3 + Xi4,  data = excel_data)
model6 <- lm(Yi~Xi1, data = excel_data)
model7 <- lm(Yi~Xi3 + Xi1, data = excel_data)
model8 <- lm(Yi~Xi4, data = excel_data)

ssr_X4 <- sum((excel_data$Yi - mean(excel_data$Yi))^2) - sum(residuals(model8)^2)
anova_table <- anova(model4, model5)
anova_table$`Sum of Sq`[2]

ssr_X1 <- sum((excel_data$Yi - mean(excel_data$Yi))^2) - sum(residuals(model6)^2)
anova_table <- anova(model4, model7)
anova_table$`Sum of Sq`[2]


#They don't equal, but they are extremely similar.


#Problem 7.27d
#             Yi        Xi1        Xi2         Xi3        Xi4
#Yi   1.00000000 -0.2502846  0.4137872  0.06652647 0.53526237
#Xi1 -0.25028456  1.0000000  0.3888264 -0.25266347 0.28858350
#Xi2  0.41378716  0.3888264  1.0000000 -0.37976174 0.44069713
#Xi3  0.06652647 -0.2526635 -0.3797617  1.00000000 0.08061073
#Xi4  0.53526237  0.2885835  0.4406971  0.08061073 1.00000000

#It means that X3 doesn't provide any help to X1 and X4 when explaining the variability in
#the model.  This can be seen by the fact that X1 and X4 have a much more varied correlation coefficient
# then X3 does.