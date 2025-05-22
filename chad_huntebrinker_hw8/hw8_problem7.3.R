#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Brand_preference_data.xlsx")

model_1 <- lm(Yi~Xi1+Xi2, data = excel_data)
sum_of_model_1 <- summary(model_1)

reduced_model_1 <- lm(Yi~Xi1, data = excel_data)

#Problem 7.3a
anova_table <- anova(reduced_model_1, model_1)
anova_table

#Problem 7.3b
#H0: p-value >= 0.01, X2 can be dropped
#H1: p-value < 0.01, X2 should not be dropped
anova_table$F[2]
anova_table$`Pr(>F)`[2]
anova_table$`Pr(>F)`[2] < 0.01
#H1 is true, don't drop X2