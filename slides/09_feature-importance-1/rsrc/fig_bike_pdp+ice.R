# PREREQ -----------------------------------------------------------------------
source("bike_data.R")
library("ggplot2")

# PLOT -------------------------------------------------------------------------
pdp <- FeatureEffects$new(predictor, method='pdp+ice')
p  = pdp$plot(ncols=3) + theme_bw()
ggsave('../figure_man/bike_pdp+ice.pdf', width=12, height=8)
