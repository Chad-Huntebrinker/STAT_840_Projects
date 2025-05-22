#Chad Huntebrinker

#Problem 3.9
x_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
residual_values <- c(3.2, 2.9, -1.7, -2.0, -2.3, -1.2, -0.9, 0.8, 0.7, 0.5)

plot(residual_values ~ x_values)
abline(h = 0, lty = 2)
