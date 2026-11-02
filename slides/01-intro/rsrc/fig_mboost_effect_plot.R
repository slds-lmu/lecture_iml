source("bikeData.R")

mboost_mod = mboost(cnt ~ bols(hum) + bols(temp) + bspatial(hum, temp), data = bike)
sink(file = "../figure/mboost_output.txt")
summary(mboost_mod)
sink(file = NULL)
png("../figure/mboost_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(mboost_mod)
dev.off()