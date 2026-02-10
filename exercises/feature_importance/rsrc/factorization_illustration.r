# Illustrating two factorizations: P(X1, X2) vs P(X1) * P(X2)
# Shows conditional vs marginal sampling for dependent and independent variables

library(ggplot2)
library(gridExtra)

set.seed(123)

# ============================================================================
# Plot 1: DEPENDENT VARIABLES
# ============================================================================

# Generate correlated (dependent) variables
n <- 500
x1_dep <- rnorm(n, 0, 1)
x2_dep <- 0.8 * x1_dep + rnorm(n, 0, 0.6)  # Strong correlation
df_dependent <- data.frame(x1 = x1_dep, x2 = x2_dep)

# Fixed value for conditioning
x1_fixed <- 0.5

# Create dependent variables plot
plot_dependent <- ggplot(df_dependent, aes(x = x1, y = x2)) +
  geom_point(alpha = 0.4, size = 0.8, color = "blue") +
  
  # Add elliptical boundary to show correlation structure
  stat_ellipse(type = "norm", level = 0.8, color = "blue", size = 1, alpha = 0.7) +
  
  # Conditional distribution curve (gray) - P(X2|X1=x1_fixed)
  stat_function(fun = function(y) x1_fixed + dnorm(y, mean = 0.8*x1_fixed, sd = 0.6) * 0.4, 
                aes(x = ..y.., y = ..x..), color = "gray40", size = 1.5, alpha = 0.8) +
  geom_path(data = data.frame(y = seq(-2.5, 2.5, 0.1), 
                              x = x1_fixed + dnorm(seq(-2.5, 2.5, 0.1), mean = 0.8*x1_fixed, sd = 0.6) * 0.4),
            aes(x = x, y = y), color = "gray40", size = 1.5, alpha = 0.8) +
  
  # Marginal distribution curve (orange) - P(X2)
  geom_path(data = data.frame(y = seq(-2.5, 2.5, 0.1), 
                              x = 2.2 + dnorm(seq(-2.5, 2.5, 0.1), mean = 0, sd = sqrt(0.8^2 + 0.6^2)) * 0.4),
            aes(x = x, y = y), color = "orange", size = 1.5, alpha = 0.8) +
  
  # Labels and annotations with background boxes
  annotate("rect", xmin = -1.4, xmax = -0.2, ymin = 0.5, ymax = 1.3, 
           fill = "white", color = "black", alpha = 0.8) +
  annotate("text", x = -0.8, y = 0.9, label = "Conditional\nP(X2|X1=x1*)", 
           color = "gray40", size = 3, hjust = 0.5) +
  annotate("rect", xmin = 2.2, xmax = 3.4, ymin = -0.2, ymax = 0.2, 
           fill = "white", color = "black", alpha = 0.8) +
  annotate("text", x = 2.8, y = 0, label = "Marginal P(X2)", 
           color = "orange", size = 3, hjust = 0.5) +
  
  labs(title = "Dependent Variables",
       subtitle = expression(paste("P(X"[1], ", X"[2], ") ", phantom() != phantom(), " P(X"[1], ") ", " P(X"[2], ")")),
       x = expression(X[1]), 
       y = expression(X[2])) +
  xlim(-3, 3.5) + ylim(-3, 3) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# ============================================================================
# Plot 2: INDEPENDENT VARIABLES  
# ============================================================================

# Generate independent variables
x1_indep <- rnorm(n, 0, 1)
x2_indep <- rnorm(n, 0, 1)  # No correlation with x1_indep
df_independent <- data.frame(x1 = x1_indep, x2 = x2_indep)

# Create independent variables plot
plot_independent <- ggplot(df_independent, aes(x = x1, y = x2)) +
  geom_point(alpha = 0.4, size = 0.8, color = "blue") +
  
  # Add rectangular boundary to show independence
  geom_rect(xmin = -2.5, xmax = 2.5, ymin = -2.5, ymax = 2.5, 
            fill = NA, color = "blue", size = 1, alpha = 0.7) +
  
  # Conditional distribution curve (gray) - P(X2|X1=x1_fixed) = P(X2) under independence
  geom_path(data = data.frame(y = seq(-2.5, 2.5, 0.1), 
                              x = x1_fixed + dnorm(seq(-2.5, 2.5, 0.1), mean = 0, sd = 1) * 0.4),
            aes(x = x, y = y), color = "gray40", size = 1.5, alpha = 0.8) +
  
  # Marginal distribution curve (orange) - P(X2) - same as conditional
  geom_path(data = data.frame(y = seq(-2.5, 2.5, 0.1), 
                              x = 2.2 + dnorm(seq(-2.5, 2.5, 0.1), mean = 0, sd = 1) * 0.4),
            aes(x = x, y = y), color = "orange", size = 1.5, alpha = 0.8) +
  
  # Arrow showing they're the same
  geom_segment(x = x1_fixed + 0.2, y = 0, xend = 2.0, yend = 0, 
               arrow = arrow(length = unit(0.15, "cm"), ends = "both"), 
               color = "black", size = 1) +
  
  # Labels and annotations with background boxes
  annotate("rect", xmin = -1.4, xmax = -0.2, ymin = 1.4, ymax = 2.2, 
           fill = "white", color = "black", alpha = 0.8) +
  annotate("text", x = -0.8, y = 1.8, label = "Conditional\nP(X2|X1=x1*)", 
           color = "gray40", size = 3, hjust = 0.5) +
  annotate("rect", xmin = 2.2, xmax = 3.4, ymin = 1.6, ymax = 2.0, 
           fill = "white", color = "black", alpha = 0.8) +
  annotate("text", x = 2.8, y = 1.8, label = "Marginal P(X2)", 
           color = "orange", size = 3, hjust = 0.5) +
  annotate("rect", xmin = 1.0, xmax = 1.5, ymin = 0.1, ymax = 0.5, 
           fill = "white", color = "black", alpha = 0.8) +
  annotate("text", x = 1.25, y = 0.3, label = "Same!", 
           color = "black", size = 3, hjust = 0.5) +
  
  labs(title = "Independent Variables",
       subtitle = expression(paste("P(X"[1], ", X"[2], ") = P(X"[1], ") ", " P(X"[2], ")")),
       x = expression(X[1]), 
       y = expression(X[2])) +
  xlim(-3, 3.5) + ylim(-3, 3) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# ============================================================================
# Save combined plot to PDF
# ============================================================================

# Save the combined plot to PDF
pdf("factorization_illustration.pdf", width = 12, height = 6)
grid.arrange(plot_dependent, plot_independent, ncol = 2)
dev.off()

cat("Plot saved to factorization_illustration.pdf\n")