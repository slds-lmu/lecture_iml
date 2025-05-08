source("bike_example_Data.R")
#remotes::install_github("schalkdaniel/compboost", "dev")
library(compboost)
options("scipen"=100, "digits"=4)

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

# create effect plot for days_since_2011
#plotBaselearnerTraces(cboost, n_legend = 10) + theme_bw()
pfe = plotPEUni(cboost, "days_since_2011") # all lines are solid :(

data1 = rbind(layer_data(pfe),layer_data(pfe, i=2))
names(data1)[1] = "Baselearner"

data1$Baselearner[data1$Baselearner==unique(data1$Baselearner)[1]] = "days_since_2011_linear"
data1$Baselearner[data1$Baselearner==unique(data1$Baselearner)[2]] = "days_since_2011_spline_centered"
data1$Baselearner[data1$Baselearner==unique(data1$Baselearner)[3]] = "Aggregated Contribution"

pfe_new = ggplot(data = data1,
                 aes(x=x, y=y, group = Baselearner, colour = Baselearner,
                     linetype = Baselearner)) + 
  geom_line(size = 1) +
  ylab("Contribution to\nprediction scroes") +
  xlab("days_since_2011")

ggsave("../figure/compboost_pfe.pdf", pfe_new, width = 7, height = 4)



