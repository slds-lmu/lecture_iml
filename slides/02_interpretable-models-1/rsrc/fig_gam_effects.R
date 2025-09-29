source("bike_example_Data.R")

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
pdf("../figure/gam_effects.pdf", width = 6, height = 5)
par(mar = c(4,4,1,1))
plot(mod_gam, pages=1, shade = TRUE)
dev.off()
