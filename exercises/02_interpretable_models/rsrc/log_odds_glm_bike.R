source("bike_example_Data.R")

library("glmnet")
library("xtable")

# data
y = bike$cnt
X = bike[,c("season", "temp", "hum", "windspeed", "days_since_2011")]
X$cnt = NULL

# create binary target variable
y_binary = ifelse(y > quantile(y, probs = 0.7), 1, 0)
dat = cbind(X, "y" = y_binary)

tab = table(dat$y, dat$season)
tabx = cbind(tab, rowSums(tab))
tabx = rbind(tabx, colSums(tabx))
xtable(tabx)

# fit logistic regression
mod_log = glm(y ~., family = binomial, data = dat)
mod_log
# glm only with feature season
mod_season = glm(y ~ season, family = binomial, data = dat)
mod_season

# Effect Table
lm_summary = summary(mod_log)$coefficients
xtable(lm_summary[,-3], digits = c(0,4,4,4))
exp(-3.21)
#
lm_season_summary = summary(mod_season)$coefficients
xtable(lm_season_summary[,-3], digits = c(0,4,4,4))
exp(-8.52)
