#Chad Huntebrinker

#Problem 3.11a

x_values <- c(-1, 0, 1, -1, 0, 1, -1, 0, 1)
residual_values <- c(0.5, 2.1, -3.4, 0.3, -1.7, 4.2, -0.6, 2.6, -4.0)

plot(residual_values ~ x_values)
abline(h = 0, lty = 2)
#They have a megaphone type

#Problem 3.11b
m.sig <- lm(residual_values^2~x_values)
anova(m.sig)
csq <- (anova(m.sig)$"Sum Sq"[1]/2)/((anova(m.sig)$"Sum Sq"[2]/9)^2)
csq > qchisq(0.95,1)
#Ha is true
