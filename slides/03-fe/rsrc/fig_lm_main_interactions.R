# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
load("../../../data/bike.RData")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# PLOT -------------------------------------------------------------------------

p1 = ggplot(data = bike, aes(x = temp, y = cnt, col = season)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("LM")

p2 = ggplot(data = bike, aes(x = temp, y = cnt, col = season)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "gam") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("GAM")

p = p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("../figure/lm_main_interactions.pdf", p, width = 8, height = 3.52)
