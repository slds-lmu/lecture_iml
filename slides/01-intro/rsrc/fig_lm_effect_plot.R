source("bikeData.R")

lm_mod = lm(cnt ~ (hum + temp)^2, data = bike)
sink(file = "../figure/lm_output.txt")
summary(lm_mod)
sink(file = NULL)
png("../figure/lm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), lm_mod), type = "response")
dev.off()
