source("bikeData.R")

glm_mod = glm(cnt ~ (hum + temp)^2, data = bike, family = Gamma(link = "inverse"))
sink(file = "../figure/glm_output.txt")
summary(glm_mod)
sink(file = NULL)
png("../figure/glm_effect_plot.png", width = 12, height = 12, units = "cm", res = 1200)
plot(Effect(c("temp"), glm_mod), type = "response")
dev.off()