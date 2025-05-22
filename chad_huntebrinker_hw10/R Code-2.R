# Chicago Redlining
# Dataset: chredlin

require(faraway)
require(leaps)
require(car)
require(onewaytests)

# To investigate charges by several Chicago community organizations that insurance companies 
# were refusing to issue insurace to racial minorities, the U.S. Commission on Civil Rights 
# gathered information on the number of FAIR plan policies written and renewed in Chicago (per
# 100 housing units, **involact**) by zip code for the months of December 1977 through May 1978.  
# FAIR plans were offered by the city of Chicago as a default policy to homeowners who had been 
# rejected by the voluntary market.  Information on other variables that might also affect insurance 
# writing were recorded. The variables are:  

# **race**, the racial composition in percentage of minority; 
# **fire**, fires per 100 housing units; 
# **theft**, thefts per 1000 population; 
# **age**, percentage of housing units built before 1939; 
# **income**, median family income in thousands of dollars; 
# **side**, North or South Side of Chicago

View(chredlin)
head(chredlin)

# The purpose of the study is to investigate the relationship between racial composition and insurance 
# refusal in Chicago between December 1977 and May 1978 while controlling for other potential sources 
# of variation.

# Fit the initial model in order to refine it
# log transformed income because impact is multiplicative and it is notoriously right-skewed
m1 <- lm(involact~race + fire + theft + age + log(income), data=chredlin)

########################################################################################################
# Exploratory data analysis to examine distribution of explanatory variables, identify outliers, and   #
# identify potential relationships between explanatory variables, and identify nature of relationships #
# between explanatory variables and outcome. Unusual values should be flagged as they may influence    #
# the fit of the model.                                                                                #  
########################################################################################################

for (i in 1:6){
  boxplot(chredlin[,i], main = names(chredlin)[i])
  stripchart(chredlin[,i], vertical = T, method = "jitter", add = TRUE)
}


# Wide range of values for **race** are observed--a good thing.
# **theft** and **fire** are right-skewed with observations clustered close to zero and a few data points 
# with large values. **theft** has an obvious outlier, zip 60607

########################################################################################################
# Screening of Explanatory Variables                                                                   #
# Partial residual plots and stepwise regression help narrow down the candidate explanatory variables. #
# Added variable plots (also known as partial residual plots or adjusted variable plots) provide       #
# evidence of the importance of a covariate given the other covariates already in the model. They also #
# display the nature of the relationship between the covariate and the outcome (i.e., linear,          #
# curvilinear, transformation necessary, etc.) and any problematic data points with respect to the     #
# predictor.                                                                                           #
########################################################################################################

# The plots all indicate no need for transformations because linear relationships are apparent.  
# They also indicate each variable provides some added value to a model that already includes all other 
# covariates because the slopes of the linear relationships are all appear to be non-zero. However, 
# **log(income)** shows points widely scattered around line so it is a potential candidate for removal.

prplot(m1,1) 
prplot(m1,2) 
prplot(m1,3) 
prplot(m1,4)
prplot(m1,5) # Drop? Points widely scattered around line

# Also problematic: **log(income)** is correlated with **race**, the explanatory variable of interest
pairs(race~fire+theft+age+log(income),data=chredlin)
cor(chredlin[,-c(5,7)])
#cor(chredlin[,c(1:7)])

# Automatic variable selection methods can be a useful starting point in eliminating redundant variables. 
# They should only be used as a guide to the screening and removal (or addition) of predictors. 
# Here, **race** is forced to stay in the model and all other covariates are allowed to add or drop: 
  
ma <- regsubsets(involact~race + fire + theft + age + log(income), force.in = 1, data = chredlin)
(sma <- summary(ma))
names(sma)
sma$rsq # bigger is better, model 3 or 4 (<1% increase for addition of log(income))
plot(3:6,sma$rsq, pch=16, xlab="Number of Parameters",ylab=expression(R[p]^2))

sma$rss # smaller is better, model 3 or 4 (<1% decrease for addition of log(income))
plot(3:6,sma$rss, pch=16, xlab="Number of Parameters",ylab=expression(SSE[p]))

sma$adjr2 # bigger is better, model 3
plot(3:6,sma$adjr2, pch=16, xlab = "Number of Parameters", ylab = expression(R[a,p]^2))

sma$cp # Cp = p is better, model 3
plot(3:6, sma$cp,  pch=16, xlab = "Number of Parameters", ylab = expression(C[p]))
abline(0,1)

sma$bic # smaller is better, model 3
plot(3:6, sma$bic,  pch=16, xlab = "Number of Parameters", ylab = expression(BIC[p]))

# Consensus evidence is to drop **log(income)** as the benefit to the 
# model is not large enough to justify the additional complexity

# Tentative Final Model
m2 <- lm(involact~race + fire + theft + age, data = chredlin)


