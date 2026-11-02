source("bike_example_Data.R")

### EXAMPLE WITH MAIN EFFECTS
mod = lm(y ~ ., data = dat, x = TRUE)

# Effect Table
lm_summary = summary(mod)$coefficients
xtable(lm_summary, digits = c(1,1,1,1,2))

# Effect Plot
p_lin_effect = effect_plot(mod, dat) + scale_x_discrete("")
ggsave("../figure/plot_lin_effect.pdf", p_lin_effect, width = 5, height = 1.75)
