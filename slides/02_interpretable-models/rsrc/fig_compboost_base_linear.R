library("mlr3verse")
library("mlr3learners")

source("bike_example_Data.R")
options("scipen"=100, "digits"=4)

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

fi = plotFeatureImportance(cboost) + theme_bw()
ggsave("../figure/compboost_pfi_base1.pdf", fi, width = 6, height = 3.5)

pred20 = cboost$predict()
mean((pred20-dat$y)^2)
# Absturz wenn man in predict() einen unpassenden datensatz übergibt
r20=plotRisk(cboost)$data$risk[21]


#### 1000 times

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
ggsave("../figure/compboost_base_linear.pdf", plot_base, width = 7, height = 3)

fi = plotFeatureImportance(cboost) + theme_bw()
ggsave("../figure/compboost_pfi_base2.pdf", fi, width = 6, height = 3.5)

r1000=plotRisk(cboost)$data$risk[1001]

# difference in risk
r20-r1000



## OOB

task = as_task_regr(dat, target = "y")
cv10 = rsmp("cv", folds = 10)
cv10$instantiate(task)
r20 = numeric()
r1000 = numeric()
for(i in 1:10){
  train_set = cv10$train_set(i)
  
  cboost = Compboost$new(data = dat[train_set,], target = "y", learning_rate = 0.1, loss = LossQuadratic$new())
  
  cboost$addBaselearner("temp", "linear", BaselearnerPolynomial, intercept = T)
  cboost$addBaselearner("hum", "linear", BaselearnerPolynomial, intercept = T)
  cboost$addBaselearner("windspeed", "linear", BaselearnerPolynomial, intercept = T)
  cboost$addBaselearner("days_since_2011", "linear", BaselearnerPolynomial, intercept = T)
  cboost$addBaselearner("season", "ridge", BaselearnerCategoricalRidge, df = 4)
  
  cboost$train(20L, trace = 20L)
  pred20 = cboost$predict(newdata = dat[cv10$test_set(i),])
  r20[i] = mean((pred20-dat[cv10$test_set(i),]$y)^2)/2
  
  cboost$train(1000L, trace = 1000L)
  pred1000 = cboost$predict(newdata = dat[cv10$test_set(i),])
  r1000[i] = mean((pred1000-dat[cv10$test_set(i),]$y)^2)/2
}
mean(r20)
mean(r1000)
mean(r20)-mean(r1000)
