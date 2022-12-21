source("bike_example_Data.R")

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
ggsave("../figure/compboost_base_linear.pdf", plot_base, width = 7, height = 3)
