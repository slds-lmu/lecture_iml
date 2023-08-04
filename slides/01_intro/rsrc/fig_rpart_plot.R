source("bikeData.R")

rpart_mod = rpart(cnt ~ hum + temp, data = bike)
sink(file = "../figure/rpart_output.txt")
rpart_mod$call
sink(file = NULL)
sink(file = "../figure/rpart_output.txt", append = TRUE)
writeLines("\n")
rpart.rules(rpart_mod)
sink(file = NULL)
png("../figure/rpart_plot.png", width = 12, height = 12, units = "cm", res = 1200)
rpart.plot(rpart_mod)
dev.off()