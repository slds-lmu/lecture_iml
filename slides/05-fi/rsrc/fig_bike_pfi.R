# PREREQ -----------------------------------------------------------------------
source("bike_data.R")
library("ggplot2")

imp <- FeatureImp$new(predictor, loss = "mae")

# PLOT -------------------------------------------------------------------------
plot(imp)
imp$results
ggsave('../figure_man/bike_pfi.pdf', width=4, height=4)