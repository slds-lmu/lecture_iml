source("bike_example_Data.R")
#remotes::install_github("schalkdaniel/compboost", "dev")
library(compboost)

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
pfe = plotPEUni(cboost, "days_since_2011") + ylab("Contribution to prediction scores") + theme_bw()
ggsave("../figure/compboost_pfe.pdf", pfe, width = 7, height = 4)
