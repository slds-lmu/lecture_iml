source("bikeData.R")

gam_mod = mgcv::gam(cnt ~ s(hum, temp), data = bike, family = gaussian)
sink(file = "../figure/gam_output.txt")
summary(gam_mod)
sink(file = NULL)

vis = getViz(gam_mod)
png("../figure/gam_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(sm(vis, 1))
dev.off()