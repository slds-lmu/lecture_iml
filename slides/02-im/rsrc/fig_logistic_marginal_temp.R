source("bike_example_Data.R")

### LASSO

library("glmnet")

# fit model with L1 regulizer
X.d = model.matrix(y ~ . + temp*season, data = X)
l.mod = glmnet(X.d, y, intercept = TRUE)
coef(l.mod)
plot(l.mod,  xvar = "lambda", ylab="Weights")

# choose best model including only 5 features
extract.glmnet.effects = function(betas, best.index) {
  data.frame(beta = betas[, best.index])
}
n.features = apply(l.mod$beta, 2, function(x){sum(x!=0)})

# create effect table
tab = extract.glmnet.effects(l.mod$beta, max(which(n.features == 6)))

# adjust intercept (stored in a0)
tab$beta[1] = l.mod$a0[ max(which(n.features == 6))]
xtable(tab, digits = 1)

####################################################################################################
# LOGISTIC REGRESSION EXAMPLE
####################################################################################################

# create binary target variable
y_binary = ifelse(y > quantile(y, probs = 0.7), 1, 0)
dat = cbind(X, "y" = y_binary)

# fit logistic regression
mod_log = glm(y ~., family = binomial, data = dat)
mod_log
table(y_binary, predict(mod_log, type = "response") > 0.5)
mean(y_binary != (predict(mod_log, type = "response") > 0.5))

# Effect Table
lm_summary = summary(mod_log)$coefficients
xtable(lm_summary[,-3], digits = c(0,2,2,2))

# create effect plot
pred = ggpredict(mod_log, terms = c("temp [-8:35 by = 1]"))
p1 = ggplot(pred, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'Class 1: high number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))
ggsave("../figure/logistic_marginal_temp.pdf", p1, width = 5, height = 3)
