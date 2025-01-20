#Chad Huntebrinker
library(readxl)
library(leaps)
library(psych)
library(lmtest)
library(dplyr)

#Load the library and data in to R
excel_data <- read.csv("realtor-data.zip.csv")

#Remove NA values
realtor_data_no_na <- na.omit(excel_data)

#Since we only want the 50 states, we need to remove Puerto Rico, Virgin Islands, Guam and Washington D.C.
#So we will have all 50 states.

realtor_data_no_na <- realtor_data_no_na[realtor_data_no_na$state != "Puerto Rico", ]
realtor_data_no_na <- realtor_data_no_na[realtor_data_no_na$state != "Virgin Islands", ]
realtor_data_no_na <- realtor_data_no_na[realtor_data_no_na$state != "Guam", ]
realtor_data_no_na <- realtor_data_no_na[realtor_data_no_na$state != "District of Columbia", ]

unique(realtor_data_no_na$state)

#Remove duplicates
realtor_data_no_na <- realtor_data_no_na[!duplicated(realtor_data_no_na), ]
data_frame_1 <- realtor_data_no_na

#Remove outliers
data_frame_2 <- subset(data_frame_1, data_frame_1$price >= 100000 & data_frame_1$price <= 1000000)
data_frame_2 <- subset(data_frame_2, data_frame_2$bed >= 1 & data_frame_2$bed <= 10)
data_frame_2 <- subset(data_frame_2, data_frame_2$bath >= 1 & data_frame_2$bath <= 10)
data_frame_2 <- subset(data_frame_2, data_frame_2$acre_lot >= 0 & data_frame_2$acre_lot <= 1)
data_frame_2 <- subset(data_frame_2, data_frame_2$house_size >= 0 & data_frame_2$house_size <= 4000)

#Most and least expensive states to live in:
most_expensive_states <- c("Pennsylvania", "Delaware", "Utah", "Alaska", "North Carolina", "Arizona",
                           "Vermont", "Texas", "New Hampshire", "Florida", "Georgia", "Illinois", "Virgina",
                           "Maryland", "Colorado", "Nevada", "Washington", "Oregon", "Rhode Island",
                           "Massachusetts", "Connecticut", "Hawaii", "New Jersey", "New York", "California")

#Add variable expensive_state to see if it is a 25 most expensive state to live in or not
data_frame_2$expensive_state <- NA
data_frame_2$expensive_state <- ifelse(data_frame_2$state %in% most_expensive_states, TRUE, FALSE)

#Take a sample of the data by tampling 50 houses per state.
set.seed(1)
sampled_df <- data_frame_2 %>%
  group_by(state) %>%
  sample_n(50) %>%
  ungroup()

#Box plots of some of the variables to see if there are any outliers
options(scipen = 999)
boxplot(sampled_df$price, ylab = "Home Price", horizontal = TRUE)
boxplot(sampled_df$bed, ylab = "Number of Beds", horizontal = TRUE)
boxplot(sampled_df$bath, ylab = "Number of Baths", horizontal = TRUE)
boxplot(sampled_df$acre_lot, ylab = "Number of Acres", horizontal = TRUE)
boxplot(sampled_df$house_size, ylab = "House size", horizontal = TRUE)


#Multicollinearity
#Remove the following predictor variables: brokered_by, status, street, city, state, and prev_sold_date
temp <- c(1, 2, 7, 8, 9, 12)
cor(data_frame_2[,-temp])

#Check to see what is the best model
#Remove the following predictor variables: brokered_by, status, street, city, state, and prev_sold_date
mr <- regsubsets(price~bed + bath + acre_lot + house_size + zip_code + expensive_state,
                 data=sampled_df)
sum_of_mr <- summary(mr)

#Check Cp, R^2, and BIC
sum_of_mr$cp
sum_of_mr$rsq
sum_of_mr$bic

#Model 4 is our best model
sum_of_mr$which[4,]
#Thus, we will include bath + house_size + zip_code + expensive_state

final_model <- lm(price~bath + house_size + zip_code + expensive_state, data = sampled_df)

plot(fitted(final_model), rstudent(final_model),
     col = "lightgreen", pch = 16,
     xlab = expression(hat(Y)),
     ylab = "Studentized Residuals",
     ylim = c(-5, 5), main = "(a)",
     cex.lab = 0.7, cex = 0.5)
abline(h = 0, lty = 2, lwd = 3,
       col = "darkgray")
abline(h = 3, lty = 3, lwd = 3,
       col = "darkgray")
abline(h = -3, lty = 3, lwd = 3,
       col = "darkgray")

#Cook's Distance
print("Value to use for Cook's Distance:")
pf(0.95, df1 = 4, df2 = 2500)

cooks_distance <- cooks.distance(final_model)
plot(cooks_distance, pch = "*", cex.main = 0.7, main = "(b)",
     ylab = "Cooks distance", cex.lab = 0.7)
text(x = 1:length(cooks_distance) + 1, y = cooks_distance, labels = ifelse(cooks_distance > 0.5660568,
                              names(cooks_distance), ""),col = "yellow")


#Homorskedasticity
plot(fitted(final_model), residuals(final_model), main = "(c)",
     xlab = expression(hat(Y)), ylab = "Residuals")
abline(h = 0, col = "gray", lwd = 2,lty = 2)

#Normality
qqnorm(residuals(final_model), main = "(d)")
qqline(residuals(final_model), col = "blue", lwd = 2)
hist(residuals(final_model), main = "(e)", xlab = "Residuals")

#Independence of error terms
plot(residuals(final_model), type = "l", col = "blue", main = "(f)", ylab = "Residuals")

#Check p-score
p_score <- coeftest(final_model)

abs(p_score[2,4]) <= 0.05
abs(p_score[3,4]) <= 0.05
abs(p_score[4,4]) <= 0.05
abs(p_score[5,4]) <= 0.05

#Validate the model
predict(final_model, data.frame(bath = 2, house_size = 1124,
                                zip_code = 35055, expensive_state = FALSE),
        level = 0.95, interval = "prediction")
print("Actual value: 169900")


predict(final_model, data.frame(bath = 2, house_size = 1398,
                                zip_code = 31558, expensive_state = TRUE),
        level = 0.95, interval = "prediction")
print("Actual value: 184900")


predict(final_model, data.frame(bath = 3, house_size = 1993,
                                zip_code = 21619, expensive_state = TRUE),
        level = 0.95, interval = "prediction")
print("Actual value: 513990")


predict(final_model, data.frame(bath = 3, house_size = 1124,
                                zip_code = 8873, expensive_state = TRUE),
        level = 0.95, interval = "prediction")
print("Actual value: 279000")


predict(final_model, data.frame(bath = 3, house_size = 3066,
                                zip_code = 29150, expensive_state = FALSE),
        level = 0.95, interval = "prediction")
print("Actual value: 369000")


predict(final_model, data.frame(bath = 2, house_size = 1500,
                                zip_code = 82941, expensive_state = FALSE),
        level = 0.95, interval = "prediction")
print("Actual value: 299900")