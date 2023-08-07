# PREREQ -----------------------------------------------------------------------

library(ggplot2)
load("data/bike.RData")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# PLOT -------------------------------------------------------------------------

p1 = ggplot(data = bike, aes(x = temp, y = cnt, fill = season)) +
  geom_point(aes(col = season), alpha = 0.5) +
  geom_smooth(aes(col = season), method = "lm", se = FALSE) + #, fullrange = TRUE) +
  labs(x = "Temperature in °C", y = "Number of bike rentals")

p2 = ggplot(data = bike, aes(x = temp, y = cnt, fill = season)) +
  geom_point(aes(col = season), alpha = 0.5) +
  geom_smooth(aes(col = season), method = "gam", se = FALSE) + #, fullrange = TRUE) +
  labs(x = "Temperature in °C", y = "Number of bike rentals")

p = gridExtra::grid.arrange(p1 + ggtitle("LM"), p2 + ggtitle("GAM"), ncol = 2)

ggsave("../figure/lm_main_seasonaleffects.pdf", p, width = 8, height = 3)
