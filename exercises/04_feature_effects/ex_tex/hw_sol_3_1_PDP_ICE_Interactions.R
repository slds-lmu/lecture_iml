## linear model with known coefficients
f_hat = function(x1, x2,
  beta0 = 0, beta1 = -8, beta2 = 0.2, beta3 = 16) {
  beta0 + beta1 * x1 + beta2 * x2 + beta3 * x1 * x2
}


## X: n*p data frame | j: column name or index | grid: vector of g values
ice = function(f, X, j, grid) {
  j = if (is.numeric(j)) names(X)[j] else j        # allow numeric or name
  g  = length(grid)
  n  = nrow(X)
  X_rep = X[rep(seq_len(n), each = g), ]           # repeat rows
  X_rep[[j]] = rep(grid, times = n)                # grid sweep on column j
  ice_mat = matrix(
    f(X_rep[[1]], X_rep[[2]]),                     # vectorised call
    nrow = n, ncol = g, byrow = TRUE
  )
  colnames(ice_mat) = grid
  ice_mat
}

pdp = function(f, X, j, grid) {
  colMeans(ice(f, X, j, grid))
}

## ref: index (1 ... g) of reference grid point
c_ice = function(f, X, j, grid, ref = 1) {
  M = ice(f, X, j, grid)
  sweep(M, 1, M[, ref])         # subtract row-specific reference
}

c_pdp = function(f, X, j, grid, ref = 1) {
  colMeans(c_ice(f, X, j, grid, ref = ref))
}


set.seed(42)
n  = 100
X  = data.frame(
  x1 = runif(n, -1, 1),          # Unif(-1,1)
  x2 = rbinom(n, 1, 0.5)         # Bernoulli(0.5)
)
grid = seq(-1, 1, by = 0.25)    # evaluation grid for x1

## compute
ICE      = ice(f_hat, X, "x1", grid)
PDP      = pdp(f_hat, X, "x1", grid)
cICE     = c_ice(f_hat, X, "x1", grid, ref = which.min(grid))
cPDP     = c_pdp(f_hat, X, "x1", grid, ref = which.min(grid))

## -------------------------------------------------
##   Reshape to long format
## -------------------------------------------------
library(ggplot2)

ice_long = data.frame(
  id   = rep(seq_len(nrow(ICE)), each = ncol(ICE)),   # row id
  x1   = rep(grid,            times = nrow(ICE)),     # grid values
  pred = as.vector(t(ICE))                            # column-wise to row-wise
)
head(ice_long)

pdp_df = data.frame(x1 = grid, pred = PDP)

## -------------------------------------------------
## Plot
## -------------------------------------------------
ggplot() +
  geom_line(data = ice_long,
    aes(x = x1, y = pred, group = id),
    colour = "grey70", linewidth = 0.3) +
  geom_line(data = pdp_df,
    aes(x = x1, y = pred),
    colour = "steelblue", linewidth = 1.2) +
  labs(title = "ICE curves (grey) and PDP (blue)",
    x = expression(x[1]),
    y = "prediction") +
  theme_minimal()




## -------------------------------------------------
##   Reshape to long format
## -------------------------------------------------
cice_long = data.frame(
  id   = rep(seq_len(nrow(cICE)), each = ncol(cICE)),   # row id
  x1   = rep(grid,            times = nrow(cICE)),     # grid values
  pred = as.vector(t(cICE))                            # column-wise to row-wise
)
head(cice_long)

cpdp_df = data.frame(x1 = grid, pred = cPDP)

## -------------------------------------------------
## Plot
## -------------------------------------------------
ggplot() +
  geom_line(data = cice_long,
    aes(x = x1, y = pred, group = id),
    colour = "grey70", linewidth = 0.3) +
  geom_line(data = cpdp_df,
    aes(x = x1, y = pred),
    colour = "steelblue", linewidth = 1.2) +
  labs(title = "c-ICE curves (grey) and c-PDP (blue)",
    x = expression(x[1]),
    y = "prediction") +
  theme_minimal()





