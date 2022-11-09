library(dplyr)
library(knitr)
library(ggpubr)
library(xtable)
library(ggeffects)
load("data/bike.RData")
source("slides/interpretable-models/rsource/helper.R")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

####################################################################################################
# LINEAR REGRESSION EXAMPLE
####################################################################################################

# Data prep
X = bike[setdiff(colnames(bike), c("yr", "weekday", "mnth", "cnt", "weathersit", "workingday", "holiday"))]
y = bike[,'cnt']
dat = cbind(X, y)


### EXAMPLE WITH MAIN EFFECTS
mod = lm(y ~ ., data = dat, x = TRUE)

# Effect Table
lm_summary = summary(mod)$coefficients
xtable(lm_summary, digits = c(1,1,1,1,2))

# Effect Plot
p_lin_effect = effect_plot(mod, dat) + scale_x_discrete("")
ggsave("slides/interpretable-models/figure/plot_lin_effect.pdf", p_lin_effect, width = 5, height = 1.75)



### EXAMPLE WITH INTERACTIONS
mod_int = lm(y ~ . + temp*season, data = dat, x = TRUE)
xtable(data.frame(mod_int$coefficients), digits = 1)


# Comparison of effects of temperature without and with interaction
mod = lm(y ~ ., data = dat)
pred = ggpredict(mod, terms = "temp")

# create main effect plot
p = ggplot(pred, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave("slides/interpretable-models/figure/main_effect_lm_temp.pdf", p, width = 4, height = 3)


# create plot with comparison of only main effect and including interaction effects
p1 = p + ggtitle("Main Effect") + xlim(-10,40) + ylim(0, 10000)

pred = ggpredict(mod_int, terms = c("temp","season"))
p2 = ggplot(pred, aes(x, predicted, colour = group)) +
  geom_point(data = dat, aes(x = temp, y = y, col = season), alpha = 0.25) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main & Interaction Effects") +
  xlim(-10,40) + ylim(0, 10000) +
  theme_bw()

p = ggarrange(p1,p2, widths = c(1, 1.2))
ggsave("slides/interpretable-models/figure/lm_main_vs_interaction_effects.pdf", p, width = 8, height = 3)



### EXAMPLE WITH POLYNOMIAL EFFECTS
mod_poly = lm(y ~ season + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)

# create effect table
xtable(data.frame(mod_poly$coefficients), digits = 1)

# create plot with comparison of only main effect and including interaction effects
pred3 = ggpredict(mod_poly, terms = c("temp"))
p3 = ggplot(pred3, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main Effect") + xlim(-10,40) + ylim(0, 10000) +
  theme_bw()

mod_poly_int = lm(y ~ season*temp + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)
pred4 = ggpredict(mod_poly_int, terms = c("temp", "season"))
p4 = ggplot(pred4, aes(x, predicted, colour = group)) +
  geom_point(data = dat, aes(x = temp, y = y, col = season), alpha = 0.25) + xlim(-10,40) + ylim(0, 10000) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main & Interaction Effects") +
  theme_bw()

p = ggarrange(p3,p4, widths = c(1, 1.2))
ggsave("slides/interpretable-models/figure/poly_main_vs_interaction_effects.pdf", p, width = 8, height = 3)




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
ggsave("slides/interpretable-models/figure/logistic_marginal_temp.pdf", p1, width = 5, height = 3)



####################################################################################################
# GAM EXAMPLE
####################################################################################################

# fit GAM with splines
dat = cbind(X,y)
mod_gam = mgcv::gam(y~season + s(temp) + s(hum) + s(windspeed) + s(days_since_2011), data = dat)

# Effect Table
gam_summary = summary(mod_gam)$s.table
xtable(gam_summary[,-c(2:3)], digits = c(0,1,2))

# create effect plot
pdf("slides/interpretable-models/figure/gam_effects.pdf", width = 6, height = 5)
par(mar = c(4,4,1,1))
plot(mod_gam, pages=1, shade = TRUE)
dev.off()


####################################################################################################
# Decision tree EXAMPLE
####################################################################################################
library(rpart)
library(rpart.plot)

# fit decision tree
tree = rpart(y~., data = dat, control = rpart.control(maxdepth = 3))

# create tree plot
pdf("slides/interpretable-models/figure/tree.pdf", width = 6, height = 4)
rpart.plot(tree)
dev.off()

# create feature importance table
xtable(data.frame(tree$variable.importance/sum(tree$variable.importance)*100))




####################################################################################################
# COMPBOOST EXAMPLE
####################################################################################################
#remotes::install_github("schalkdaniel/compboost", "dev")
library(compboost)

# Linear baselearner example
set.seed(31415)
cboost = Compboost$new(data = dat, target = "y", learning_rate = 0.1, loss = LossQuadratic$new())

cboost$addBaselearner("temp", "linear", BaselearnerPolynomial, intercept = T)
cboost$addBaselearner("hum", "linear", BaselearnerPolynomial, intercept = T)
cboost$addBaselearner("windspeed", "linear", BaselearnerPolynomial, intercept = T)
cboost$addBaselearner("days_since_2011", "linear", BaselearnerPolynomial, intercept = T)
cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)
# cboost$addIntercept()
# cboost$addBaselearner("temp", "linear", BaselearnerPolynomial, intercept = FALSE)
# cboost$addBaselearner("hum", "linear", BaselearnerPolynomial, intercept = FALSE)
# cboost$addBaselearner("windspeed", "linear", BaselearnerPolynomial, intercept = FALSE)
# cboost$addBaselearner("days_since_2011", "linear", BaselearnerPolynomial, intercept = FALSE)
# cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)

library(data.table)
cboost$train(20L, trace = 20L)
coefs = cboost$getCoef()

intercept = sapply(coefs, function(x) ifelse(length(x) <= 2, x[1], NA))
slope = vapply(coefs, function(x) {
  if(length(x) > 2) {
    return(paste0(rownames(x), ": ", round(x, digits = 1), collapse = ", "))
    #return(setNames(x[1:length(x)], rownames(x)))
  } else {
    if(length(x) == 2) {
      return(as.character(round(x[2], digits = 1)))
    } else {
      return(NA_character_)
    }
  }}, character(1))

df = data.frame(
  "Feature" = gsub("_linear|_ridge","", names(coefs)),
  "Intercept" = intercept,
  "Weights" = slope)
row.names(df) = df$Feature
df$Feature = NULL
xtable(df)



cboost$train(1000L, trace = 1000L)
coefs = cboost$getCoef()

intercept = sapply(coefs, function(x) ifelse(length(x) <= 2, x[1], NA))
slope = vapply(coefs, function(x) {
  if(length(x) > 2) {
    return(paste0(rownames(x), ": ", round(x, digits = 1), collapse = ", "))
    #return(setNames(x[1:length(x)], rownames(x)))
  } else {
    if(length(x) == 2) {
      return(as.character(round(x[2], digits = 1)))
    } else {
      return(NA_character_)
    }
  }}, character(1))

df = data.frame(
  "Feature" = gsub("_linear|_ridge","", names(coefs)),
  "Intercept" = intercept,
  "Weights" = slope)
row.names(df) = df$Feature
df$Feature = NULL
xtable(df)


plot_base = plotBaselearnerTraces(cboost) + theme_bw()
ggsave("slides/interpretable-models/figure/compboost_base_linear.pdf", plot_base, width = 7, height = 3)

# fit compboost model with linear and centered splines for numeric features and categorical
# base learner for season
set.seed(31415)
cboost = Compboost$new(data = dat, target = "y", learning_rate = 0.1,
  loss = LossQuadratic$new())#, oob_fraction = 0.2)

cboost$addComponents("temp", df = 4)
cboost$addComponents("hum", df = 4)
cboost$addComponents("windspeed", df = 4)
cboost$addComponents("days_since_2011", df = 4)
cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)

cboost$train(1000L, trace = 1000L)

# create feature importance plot
fi = plotFeatureImportance(cboost) + theme_bw()
ggsave("slides/interpretable-models/figure/compboost_pfi.pdf", fi, width = 6, height = 3.5)

# create effect plot for days_since_2011
#plotBaselearnerTraces(cboost, n_legend = 10) + theme_bw()
pfe = plotPEUni(cboost, "days_since_2011") + ylab("Contribution to prediction scores") + theme_bw()
ggsave("slides/interpretable-models/figure/compboost_pfe.pdf", pfe, width = 7, height = 4)
