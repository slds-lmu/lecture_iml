# ---------------------------------------------
# Logistic-regression effect of temperature
# ---------------------------------------------
library(ggplot2)
library(gridExtra)

## coefficients
beta0 <- -8.52 + 0.9426783
beta1 <-  0.29

## temperature grid
Tseq <- -5:35
eta  <- beta0 + beta1 * Tseq
p    <- 1 / (1 + exp(-eta))                # predicted probability
dp   <- p * (1 - p) * beta1               # marginal effect

## data frame
df <- data.frame(
    temp = Tseq,
    prob = p,
    dprob_dT = dp
)

## plot
p1 = ggplot(df, aes(temp, prob)) +
    geom_line(color = "steelblue4", size = 1.2) +
    labs(
        x = "Temperature (°C)",
        y = "P(high rentals)",
        title = "Probability of High Bike Rentals vs. Temperature"
    ) + ylim(0,1) +
    theme_minimal(base_size = 12)

## plot marginal change
p2 = ggplot(df, aes(temp, dprob_dT)) +
    geom_line(color = "firebrick", size = 1.2) +
    labs(
        x = "Temperature (°C)",
        y = "dp/dx1",
        title = "Instantaneous Change in Probability per +1 C"
    ) +
    theme_minimal(base_size = 12)

grid.arrange(p1, p2, nrow = 2)
