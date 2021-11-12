# PREREQ -----------------------------------------------------------------------

library(ggplot2)
load("data/bike.RData")
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# PLOT -------------------------------------------------------------------------

p1 = ggplot(data = bike, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = bike, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "gam") +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5))

p = gridExtra::grid.arrange(p1 + ggtitle("LM"), p2 + ggtitle("GAM"), ncol = 2)

ggsave("slides/feature-effects/figure_man/lm_main_effects.pdf", p, width = 8, height = 3)