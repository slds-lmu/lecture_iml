library(dplyr)
library(knitr)
library(ggpubr)
library(xtable)
library(ggeffects)
library(rpart)
library(mgcv)
library(randomForest)
library(gbm)
library(mlbench)
source("slides/02_interpretable-models/rsrc/helper.R")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# Bike data
load("data/bike.RData")

# Boston Housing Data
data(BostonHousing)


create_tab = function(x, y){
  dat = data.frame(y,x)
  n_dat = length(dat$y)
  test_id = sample(1:n_dat, n_dat*0.33)
  test = dat[test_id,]
  dat = dat[-test_id,]
  
  rmse = numeric()
  r_sqrt = numeric()
  var = sum((test$y-mean(test$y))^2)
  
  ### Linear Model
  lm = lm(y ~ ., data = dat)
  y_lm = predict(lm, newdata = test)
  rmse[1] = sqrt(mean((test$y - y_lm)^2))
  r_sqrt[1] = sum((y_lm-mean(test$y))^2)/var
  
  
  ### Tree
  tree = rpart(y ~., data = dat)
  y_tree = predict(tree, newdata = test)
  rmse[2] = sqrt(mean((test$y - y_tree)^2))
  r_sqrt[2] = sum((y_tree-mean(test$y))^2)/var
  
  
  ### GAM
  gam = gam(formula = y ~ ., data = dat)
  y_gam = predict(gam, newdata = test)
  rmse[3] = sqrt(mean((test$y - y_gam)^2))
  r_sqrt[3] = sum((y_gam-mean(test$y))^2)/var
  
  
  ### Random Forest
  rf = randomForest(y ~., data = dat)
  y_rf = predict(rf, newdata = test)
  rmse[4] = sqrt(mean((test$y - y_rf)^2))
  r_sqrt[4] = sum((y_rf-mean(test$y))^2)/var
  
  
  ### Boosting
  boost = gbm(y ~., data = dat)
  y_boost = predict(boost, newdata = test)
  rmse[5] = sqrt(mean((test$y - y_boost)^2))
  r_sqrt[5] = sum((y_boost-mean(test$y))^2)/var
  
  
  #### table
  tab = data.frame(Model = c("LM", "Tree", "GAM", "Random Forest", "Boosting"),
                   RMSE = rmse,
                   R2 = round(r_sqrt,3))
  
  return(tab)
}

### create tabs
# bike data
tab = create_tab(y = bike$cnt, x = subset(bike, select = -c(cnt)))
xtable(tab, type = "latex", tabular.environment="longtable")

# Boston Housing data
tab = create_tab(y = BostonHousing$medv, x = subset(BostonHousing, select = -c(medv)))
xtable(tab, type = "latex", tabular.environment="longtable")
