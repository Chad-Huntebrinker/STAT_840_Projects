#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Commercial_properties_data.xlsx")

model_2 <- lm(Yi~Xi1+Xi2+Xi3+Xi4, data = excel_data)
sum_of_model_2 <- summary(model_2)
sum_of_model_2

#Problem 7.7a
model_Xi4 <- lm(Yi ~ Xi4, data = excel_data)
model_Xi1_Xi4 <- lm(Yi ~ Xi1 + Xi4, data = excel_data)
model_Xi2_Xi1_Xi4 <- lm(Yi ~ Xi2 + Xi1 + Xi4, data = excel_data)

anova_table_Xi4 <- anova(model_Xi4, model_Xi1_Xi4)
anova_table_Xi2_Xi1_Xi4 <- anova(model_Xi1_Xi4, model_Xi2_Xi1_Xi4)
anova_table_Xi3_Xi2_Xi1_Xi4 <- anova(model_Xi2_Xi1_Xi4, model_2)

anova_table_Xi4
anova_table_Xi2_Xi1_Xi4
anova_table_Xi3_Xi2_Xi1_Xi4

#7.7b
#H0: p-value >= 0.01, X3 can be dropped
#H1: p-value < 0.01, X3 should not be dropped
anova_table_Xi3_Xi2_Xi1_Xi4$F[2]
anova_table_Xi3_Xi2_Xi1_Xi4$`Pr(>F)`[2]
anova_table_Xi3_Xi2_Xi1_Xi4$`Pr(>F)`[2] < 0.01
#H1 is false, so can drop X3

#Problem 7.8
anova_table_Xi1_Xi4 <- anova(model_Xi1_Xi4, model_2)
anova_table_Xi1_Xi4

#H0: p-value >= 0.01, X2 and X3 can be dropped
#H1: p-value < 0.01, X2 and X3 should not be dropped
anova_table_Xi1_Xi4$F[2]
anova_table_Xi1_Xi4$`Pr(>F)`[2]
anova_table_Xi1_Xi4$`Pr(>F)`[2] < 0.01
#H1 is true, so you shouldn't drop X2 and X3

