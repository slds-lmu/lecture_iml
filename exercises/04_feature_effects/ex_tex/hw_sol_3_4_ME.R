# R implementation for dME, fME, and NLM

# Define the model function
f_hat = function(x1, x2, theta0 = 0, theta1 = 1, theta2 = 0.5, theta12 = 2) {
  return(theta0 + theta1 * x1^2 + theta2 * x2^2 + theta12 * x1 * x2)
}

# Numerical derivative Marginal Effect (central difference)
dME = function(f, x, j, h = 1e-5) {
  x_forward = x
  x_backward = x
  x_forward[j] = x_forward[j] + h
  x_backward[j] = x_backward[j] - h
  return((f(x_forward[1], x_forward[2]) - f(x_backward[1], x_backward[2])) / (2 * h))
}

# analytical derivative for this example
dMEanalytical = function(x, theta1 = 1, theta12 = 2) {
  2*theta1*x[1] + theta12*x[2]
}

# Forward Marginal Effect
fME = function(f, x, h_vec) {
  x_perturbed = x + h_vec
  return(f(x_perturbed[1], x_perturbed[2]) - f(x[1], x[2]))
}

# Non-Linearity Measure (NLM)
NLM = function(f, x, h_vec, t_vals = seq(0.1, 0.9, length = 9)) {
  x_path = sapply(t_vals, function(t) x + t * h_vec)
  f_vals = apply(x_path, 2, function(pt) f(pt[1], pt[2]))
  f_bar = mean(f_vals)
  fme_val = fME(f, x, h_vec)
  g_vals = sapply(t_vals, function(t) f(x[1], x[2]) + t * fme_val)
  num = sum((f_vals - g_vals)^2)
  den = sum((f_vals - f_bar)^2)
  return(1 - num / den)
}

library(ggplot2)

# Base point and step
x      = c(1, 2)
h_vec  = c(0.5, 0)          # step in x1 only
h1     = h_vec[1]

# dME / fME / NLM values
dme_val = dME(f_hat, x, j = 1)
dme_val
dMEanalytical(x)          # analytic derivative

fme_val = fME(f_hat, x, h_vec)
fme_val

nlm_val = NLM(f_hat, x, h_vec)
nlm_val

# Prediction curve in neighbourhood
x_seq = seq(x[1] - 0.5, x[1] + h1 + 0.5, length.out = 400)
curve_df = data.frame(
  x  = x_seq,
  y  = f_hat(x_seq, x[2])
)

# Tangent line (dashed)
tan_df = data.frame(
  x = x_seq,
  y = f_hat(x[1], x[2]) + dme_val * (x_seq - x[1])
)

# Secant line (dotted)
sec_df = data.frame(
  x = x_seq,
  y = f_hat(x[1], x[2]) + fme_val / h1 * (x_seq - x[1])
)

# Start / end points
pts_df = data.frame(
  x = c(x[1], x[1] + h1),
  y = c(f_hat(x[1], x[2]), f_hat(x[1] + h1, x[2])),
  lbl = c("start", "end")
)

# Plot
ggplot(curve_df, aes(x, y)) +
  geom_line(size = 1, colour = "black") +
  geom_line(data = tan_df, linetype = "dashed") +
  geom_line(data = sec_df, linetype = "dotted") +
  geom_point(data = pts_df, size = 3) +
  geom_text(data = pts_df, aes(label = lbl), vjust = -1) +
  labs(title = "Prediction curve with tangent (dashed) and secant (dotted)",
    subtitle = sprintf("dME = %.2f, fME = %.2f, NLM (R²) = %.4f",
      dme_val, fme_val, nlm_val),
    x = expression(x[1]), y = expression(hat(f)(x[1],2))) +
  theme_minimal(base_size = 12)
