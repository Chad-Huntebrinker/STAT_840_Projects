#Chad Huntebrinker

#Load the library and data in to R
library(readxl)
excel_data <- read_excel("SENIC_Data.xlsx")

#Length_of_Stay ~ Infection_Risk
#Fit the model
model_6 <- lm(Length_of_Stay~Infection_Risk,data=excel_data)
sum_of_model_6 <- summary(model_6)

#Plot to see the graph
plot(Length_of_Stay~Infection_Risk, data = excel_data)
abline(model_6)

#Problem 4.27a
#1 - alpha/(2*m) [where m = 2 since we need one for B0 and one for B1]
1-0.1/4
confint(model_6, level = 0.975)
#We have the joint confidence interval for as 0.5003816 <= B1 <= 1.020460
#and 5.1523372 <= B0 <= 7.521236

#Problem 4.27b
#The researcher is saying that B0 should be about 7 and B1 should be about 1.
#According to Problem 2.27a, this is supported. Both B0=7 and B1=1 are in the
#confidence interval.  Thus, we can say that this is true when the family confidence
#coefficient is at least 0.9 that the procedure leads to correct pairs of interval estimates.

#Problem 4.27c and d
W <- sqrt(2*qf(1-0.05,2,113-2))
B <- qt(1-0.05/8, 113-2)
B > W
#The more efficient method should be the Working-Hotelling. The reason being that we have
#a larger value of m (m = 4) which means the Working-Hotelling will be more precise. We
#also see that the W value is less than the B value, leading us to believe that the
#Working-Hotelling method will lead to a tighter confidence limits.
#But we'll calculate using both just to see.

X.h <- c(2, 3, 4, 5)

family_con <- 1-0.05/8

# Estimation
CI.bf <- predict(model_6,data.frame(Infection_Risk = X.h), interval="confidence",level=family_con) # Adjust level

# Working-Hotelling multiplier for 95% simultaneous confidence band
# Extract standard errors of Y-hat
CI.wh <- predict(model_6,data.frame(Infection_Risk = X.h), se.fit = TRUE, interval = "confidence", 
              level = 0.95)
wh.LB <- CI.wh$fit[,1] - W*CI.wh$se.fit # WH lower bound
wh.UB <- CI.wh$fit[,1] + W*CI.wh$se.fit # WH upper bound

#Bonferroni:
#2:  6.994082 <-> 8.721175: dif = 1.7270924 
#3:  8.011273 <-> 9.224825: dif = 1.2135524 
#4:  8.937812 <->  9.819129: dif = 0.8813172 
#5:  9.665901 <-> 10.611881: dif = 0.9459808 

#Working-Hotelling:
#2: 7.088991 <-> 8.626266: dif = 1.537275
#3: 8.077961 <-> 9.158137: dif = 1.080176
#4: 8.986242 <-> 9.770698: dif = 0.784456
#5: 9.717885 <-> 10.559897: dif = 0.842012