source("bike_example_Data.R")

### EXAMPLE WITH POLYNOMIAL EFFECTS
mod_poly = lm(y ~ season + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)

# create effect table
xtable(data.frame(mod_poly$coefficients), digits = 1)

# create plot with comparison of only main effect and including interaction effects
pred3 = ggpredict(mod_poly, terms = c("temp"))
p3 = ggplot(pred3, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main Effect") + xlim(-10,40) + ylim(0, 10000) +
  theme_bw()

mod_poly_int = lm(y ~ season*temp + hum + windspeed + days_since_2011 + poly(temp, 2, raw = TRUE), data = dat)
pred4 = ggpredict(mod_poly_int, terms = c("temp", "season"))
p4 = ggplot(pred4, aes(x, predicted, colour = group)) +
  geom_point(data = dat, aes(x = temp, y = y, col = season), alpha = 0.25) + xlim(-10,40) + ylim(0, 10000) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "", col = "Season") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Main & Interaction Effects") +
  theme_bw()

p = ggarrange(p3,p4, widths = c(1, 1.2))
ggsave("../figure/poly_main_vs_interaction_effects.pdf", p, width = 8, height = 3)

