library(dplyr)
library(knitr)
library(ggpubr)
library(xtable)
library("ggeffects")
load("slides/interpretable-models/rsource/bike.RData")
source("slides/interpretable-models/rsource/helper.R")


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
xtable(lm_summary, digits = c(0,0,0,0,2))

# Effect Plot
p_lin_effect = effect_plot(mod, dat) + scale_x_discrete("")
ggsave("slides/interpretable-models/figure/plot_lin_effect.pdf", p_lin_effect, width = 5, height = 3)



### EXAMPLE WITH INTERACTIONS
mod_int = lm(y ~ . + temp*season, data = dat, x = TRUE)
xtable(data.frame(mod_int$coefficients))


# Comparison of effects of temperature without and with interaction
mod = lm(y ~ ., data = dat)
pred = ggpredict(mod, terms = "temp")

# create main effect plot
p = ggplot(pred, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in ?C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave("slides/interpretable-models/figure/main_effect_lm_temp.pdf", p, width = 4, height = 3)


# create plot with comparison of only main effect and including interaction effects
p1 = p + ggtitle("Main Effect") 

pred = ggpredict(mod_int, terms = c("temp","season"))
p2 = ggplot(pred, aes(x, predicted, colour = group)) +
  geom_point(data = dat, aes(x = temp, y = y, col = season), alpha = 0.25) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in ?C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main & Interaction Effects") +
  theme_bw()

p = ggarrange(p1,p2)
ggsave("slides/interpretable-models/figure/lm_main_vs_interaction_effects.pdf", p, width = 8, height = 3)



### EXAMPLE WITH POLYNOMIAL EFFECTS
mod_poly = lm(y ~ season + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)

# create effect table
xtable(data.frame(mod_poly$coefficients))

# create plot with comparison of only main effect and including interaction effects
pred3 = ggpredict(mod_poly, terms = c("temp"))
p3 = ggplot(pred3, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in ?C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main Effect") +
  theme_bw()

mod_poly_int = lm(y ~ season*temp + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)
pred4 = ggpredict(mod_poly_int, terms = c("temp", "season"))
p4 = ggplot(pred4, aes(x, predicted, colour = group)) +
  geom_point(data = dat, aes(x = temp, y = y, col = season), alpha = 0.25) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in ?C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main & Interaction Effects") +
  theme_bw()

p = ggarrange(p3,p4)
ggsave("slides/interpretable-models/figure/poly_main_vs_interaction_effects.pdf", p, width = 8, height = 3)




### LASSO

library("glmnet")

# fit model with L1 regulizer
X.d = model.matrix(y ~ season + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = X)
l.mod = glmnet(X.d, y, intercept = TRUE)
coef(l.mod)
plot(l.mod,  xvar = "lambda", ylab="Weights")

# choose best model including only 5 features
extract.glmnet.effects = function(betas, best.index) {
  data.frame(beta = betas[, best.index])
}
n.features = apply(l.mod$beta, 2, function(x){sum(x!=0)})

# create effect table
xtable(extract.glmnet.effects(l.mod$beta, max(which(n.features == 5))))




####################################################################################################
# LOGISTIC REGRESSION EXAMPLE
####################################################################################################

# create binary target variable
y_binary = ifelse(y > quantile(y, probs = 0.7), 1, 0)
dat = cbind(X, "y" = y_binary)

# fit logistic regression
mod_log = glm(y ~., family = binomial, data = dat)


# Effect Table
lm_summary = summary(mod_log)$coefficients
xtable(lm_summary, digits = c(0,1,1,1,2))

# create effect plot
pred = ggpredict(mod_log, terms = c("temp [-8:35 by = 1]"))
p1 = ggplot(pred, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in ?C", y = "Marginal Effect on \n 'Class 1: high number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))
ggsave("slides/interpretable-models/figure/logistic_maginal_temp.pdf", p1, width = 5, height = 3)



####################################################################################################
# GAM EXAMPLE
####################################################################################################

# fit GAM with splines
dat = cbind(X,y)
mod_gam = mgcv::gam(y~season + s(temp) + s(hum) + s(windspeed) + s(days_since_2011), data = dat)

# Effect Table
gam_summary = summary(mod_gam)$s.table
xtable(gam_summary, digits = c(0,1,1,1,2))

# create effect plot
pdf("slides/interpretable-models/figure/gam_effects.pdf", width = 6, height = 5)
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
#devtools::install_github("schalkdaniel/compboost")
library(compboost)

# fit compboost model with linear and centered splines for numeric features and categorical 
# base learner for season
set.seed(31415)
cboost = Compboost$new(data = dat, target = "y", learning_rate = 0.02,
                       loss = LossQuadratic$new(), oob_fraction = 0.2)

cboost$addComponents("temp", df = 4)
cboost$addComponents("hum", df = 4)
cboost$addComponents("windspeed", df = 4)
cboost$addComponents("days_since_2011", df = 4)
cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)

cboost$train(500L, trace = 100L)

# create feature importance plot
fi = plotFeatureImportance(cboost) + theme_bw()
ggsave("slides/interpretable-models/figure/compboost_pfi.pdf", fi, width = 6, height = 3.5)

# create effect plot for days_since_2011
pfe = plotPEUni(cboost, "days_since_2011") + ylab("Contribution to prediction scores") + theme_bw()
ggsave("slides/interpretable-models/figure/compboost_pfe.pdf", pfe, width = 7, height = 4)


