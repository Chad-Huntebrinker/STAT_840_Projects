x_data <- c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0)
y_data <- c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)

#Plot the Data
plot(y_data ~ x_data, pch = 16, xlab = "Number of Transfers", ylab = "Number of Broken Ampules")
minor.tick(nx = 2, tick.ratio = 0.5)

#Fit a Linear Regression model
lrgm <- lm(y_data ~ x_data)

#Find b coefficients
b_coefficients <- lrgm$coefficients

#Add the fitted line
abline(b_coefficients, lwd =2, lty =2, col="red")
