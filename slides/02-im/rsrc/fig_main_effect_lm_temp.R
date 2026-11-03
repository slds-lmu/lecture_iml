source("bike_example_Data.R")

### EXAMPLE WITH INTERACTIONS
mod_int = lm(y ~ . + temp*season, data = dat, x = TRUE)
xtable(data.frame(mod_int$coefficients), digits = 1)


# Comparison of effects of temperature without and with interaction
mod = lm(y ~ ., data = dat)
pred = ggpredict(mod, terms = "temp")

# create main effect plot
p = ggplot(pred, aes(x, predicted)) +
  geom_point(data = dat, aes(x = temp, y = y), alpha = 0.25) +
  geom_line(col = "blue", size = 1) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "Temperature in °C", y = "Marginal Effect on \n 'number of bike rentals'") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave("../figure/main_effect_lm_temp.pdf", p, width = 4, height = 3)