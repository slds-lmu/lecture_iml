library(ggpubr)
library(ggcorrplot)
load("../../../data/bike.RData")
theme_set(theme_bw())

p =   ggplot(data = bike, aes(x=temp, y = cnt)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "#00BFC4") +
    labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals"))

ggsave("../figure/intro_lm_bike.pdf", p, width = 10, height = 5)
