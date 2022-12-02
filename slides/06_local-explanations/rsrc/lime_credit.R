library(knitr)
library(mlr)
library(partykit)
library(vcd)
library(iml)
library(gridExtra)
library(gower)
library(ggplot2)
library(ggpubr)

set.seed(123)
source("helpers.R")
pred = readRDS("model_svm.rds")

###---- Get data ----
credit = read.csv("german_credit_data.csv", row.names = 1, stringsAsFactors = TRUE)
names(credit)
# omit rows with NA entries
credit = na.omit(credit)
# join groups with small frequencies
levels(credit$Purpose) = c("others", "car", "others", "others",
  "furniture", "radio/TV", "others", "others")
levels(credit$Saving.accounts) = c("little", "moderate", "rich", "rich")
# Colnames to lower
names(credit) = tolower(names(credit))
# Drop levels
credit = droplevels.data.frame(credit)

###---- Define xinterest and training/test data ----
# Separate xinterest from training dataset
x.interest = credit[1,]
x.interest$risk <- NULL
credit = credit[-1,]

###--- PDP plots or ICE curves ----
ice = get_ice_curve_area(instance = x.interest, features = c("duration", "credit.amount"), predictor = pred, grid.size = 50L)
plot = plot_ice_curve_area(grid = ice, predictor = pred, instance = x.interest, x.interest = x.interest)
###--- Train model-----
# set.seed(123)
# task = makeClassifTask(id = "credit",
#   data = credit, target = "risk")
# lrn = makeLearner("classif.svm", predict.type = "prob")
# mod = train(lrn, task)
### use trained model (same also for counterfactuals)
mod = readRDS("model_svm.rds")
pred = Predictor$new(mod, data = credit, class = "bad")

###--- Get explanation----
x.interest
pred$predict(x.interest)
n_features_lime = 5
lim = LocalModel$new(pred, x.interest = x.interest, k = n_features_lime, dist.fun = "gower")
a = plot(lim)
a
ggsave(plot = a, filename = "../figure/lime_credit.pdf", height = 2.5, width = 4.5)

# local fidelity
pi =  lim$predict(newdata = credit)[, 1]
pit = pred$predict(newdata = credit)[, 1]
gow = 1 - gower_dist(x.interest, credit)
sum(gow*(pi - pit)^2)
##################################################
# new ice curve
ice = get_ice_curve_area(instance = x.interest, features = c("duration", "credit.amount"), predictor = lim, grid.size = 50L)
newplot = plot_ice_curve_area(grid = ice, predictor = pred, instance = x.interest, x.interest = x.interest)
ggsave(plot = plot, filename = "../figure/lime_credit_ice1.pdf", height = 3, width = 4)
ggsave(plot = newplot, filename = "../figure/lime_credit_ice2.pdf", height = 3, width = 4)
