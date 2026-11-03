# PREREQ -----------------------------------------------------------------------

library(ggplot2)
library(patchwork)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))
source("fig_ale_scatter.R")


## PACKAGES ------------------------------------------------------------------
required_pkgs <- c("ggplot2", "plot3D", "plot3Drgl", "htmlwidgets", "dplyr")
unused       <- lapply(setdiff(required_pkgs, rownames(installed.packages())),
  function(p) install.packages(p, quiet = TRUE))
lapply(required_pkgs, library, character.only = TRUE)

theme_set(
  theme_bw() + 
    theme(plot.margin = grid::unit(c(1, 5.5, 1, 1), "pt"))
)

## 1  SYNTHETIC DATA  ---------------------------------------------------------
set.seed(10)
n  <- 50
x1 <- runif(n, -5, 5)
x2 <- x1 + rnorm(n, 0, 1)                  # introduce correlation

df <- tibble::tibble(x1, x2)

## Smooth nonlinear response surface (bonus requirement)
f_hat <- function(x1, x2) {                # “black-box” model
  #-0.1*(x1)^2*sin(0.5*x1) + 0.5 * cos(x2) + 0.1 * x1 * ifelse(x2<0, -1*sqrt(abs(x2)), sqrt(abs(x2)))
  sin(x1) + 0.5*cos(x2) + 0.2*x1*x2
}
df$yhat <- f_hat(df$x1, df$x2)

## 2  GLOBAL OBJECTS  ---------------------------------------------------------
K         <- 4                             # number of ALE intervals
breaks_x1 <- quantile(df$x1, probs = seq(0, 1, length.out = K + 1))
# Small helper for 3-D mesh
grid_res  <- 60
x1_grid   <- seq(min(df$x1), max(df$x1), length.out = grid_res)
x2_grid   <- seq(min(df$x2), max(df$x2), length.out = grid_res)
z_mat     <- outer(x1_grid, x2_grid, f_hat)

###############################################################################
# STEP 1  – Interval Partitioning                                             #
###############################################################################
# 2-D illustration ------------------------------------------------------------
g_base <- ggplot(df, aes(x1, x2)) +
  geom_point(size = 1) +
  geom_vline(xintercept = breaks_x1, colour = "black") +
  labs(x = expression(X[1]), y = expression(X[2]),
    title = "Step 1 – Interval partitioning (2-D)") +
  scale_x_continuous(sec.axis = dup_axis(breaks = breaks_x1,
    labels = paste0("z[", 0:K, ",1]")))
ggsave("../figure/ale_step1_2D.pdf", g_base, width = 5, height = 4)

###############################################################################
# STEP 2  – Finite Differences at Interval j = 1                              #
###############################################################################
j      <- 1                                # focus on first interval
z_low  <- breaks_x1[j]
z_high <- breaks_x1[j + 1]
id_j   <- df$x1 >= z_low & df$x1 < z_high

## calculate ± substitutions & differences
df_sub           <- df[id_j, ]
df_sub$x1_low    <- z_low
df_sub$x1_high   <- z_high
df_sub$y_low     <- f_hat(df_sub$x1_low, df_sub$x2)
df_sub$y_high    <- f_hat(df_sub$x1_high, df_sub$x2)
df_sub$delta     <- df_sub$y_high - df_sub$y_low

# 2-D illustration ------------------------------------------------------------
g_fd <- g_base +
  geom_segment(data = df_sub,
    aes(x = x1_low, xend = x1_high,
      y = x2,     yend = x2),
    colour = "blue") +
  geom_point(data = df_sub, aes(x1_low,  x2), colour = "blue") +
  geom_point(data = df_sub, aes(x1_high, x2), colour = "blue") +
  labs(title = "Step 2 – Finite differences (2-D)") +
  annotate("text", x = mean(c(z_low, z_high)), y = max(df$x2),
    label = expression(Delta[f]), vjust = -0.2, colour = "blue")
ggsave("../figure/ale_step2_2D.pdf", g_fd, width = 5, height = 4)

###############################################################################
# STEP 3  – Averaging within the interval                                     #
###############################################################################
delta_bar <- mean(df_sub$delta)

# 2-D illustration ------------------------------------------------------------
g_avg <- g_fd +
  geom_segment(aes(x = z_low, xend = z_high,
    y = min(df$x2) - 1, yend = min(df$x2) - 1),
    colour = "red", size = 1.1) +
  annotate("text", x = mean(c(z_low, z_high)),
    y = min(df$x2) - 2.2,
    label = bquote(bar(Delta[f]) == .(format(delta_bar, digits = 3))),
    colour = "red") +
  labs(title = "Step 3 – Average finite difference (2-D)")
ggsave("../figure/ale_step3_2D.pdf", g_avg, width = 5, height = 4)

###############################################################################
# STEP 4  – Accumulation over intervals                                       #
###############################################################################
# For every interval compute averaged delta and cumulative sum (centred ALE)
ale_tbl <- purrr::map_dfr(seq_len(K), function(j) {
  id      <- df$x1 >= breaks_x1[j] & df$x1 < breaks_x1[j + 1]
  delta   <- with(df[id, ], f_hat(breaks_x1[j + 1], x2) -
      f_hat(breaks_x1[j],     x2))
  tibble::tibble(
    j         = j,
    z_low     = breaks_x1[j],
    z_high    = breaks_x1[j + 1],
    delta_bar = mean(delta)
  )
}) %>%
  mutate(ale = cumsum(delta_bar),
    z_mid = 0.5 * (z_low + z_high))
#ale_tbl$ale <- ale_tbl$ale - mean(ale_tbl$ale)   # centre ALE at zero

# 2-D illustration ------------------------------------------------------------
g_acc <- ggplot(ale_tbl, aes(z_mid, ale)) +
  geom_step(direction = "mid") +
  geom_point(colour = "red", size = 2) +
  geom_segment(aes(xend = z_mid, yend = 0), linetype = "dotted") +
  labs(title = "Step 4 – Accumulated local effects (2-D)",
    x = expression(X[1]), y = "ALE( x₁ )")
ggsave("../figure/ale_step4_2D.pdf", g_acc, width = 5, height = 4)

###############################################################################
# END OF SCRIPT                                                               #
###############################################################################








library(reticulate)
#reticulate::conda_install('r-reticulate', 'python-kaleido')
###############################################################################
# 3-D ALE VISUALISATION – plotly  (static PDF, no helper functions)           #
###############################################################################
library(plotly)      # ← only external dependency
library(dplyr)       # just for tibble / mutate
# 
# ## 1  SYNTHETIC DATA ----------------------------------------------------------
# set.seed(10)
# n  <- 50
# x1 <- runif(n, -5, 5)
# x2 <- x1 + rnorm(n, 0, 1)                     # correlated features
# df <- tibble(x1, x2)
# 
# f_hat <- function(x1, x2) {                   # smooth response surface
#   sin(x1) + 0.5*cos(x2) + 0.2*x1*x2
# }
# df$yhat <- f_hat(df$x1, df$x2)
# 
# ## 2  GRIDS & CONSTANTS -------------------------------------------------------
K         <- 4
breaks_x1 <- quantile(df$x1, probs = seq(0, 1, length.out = K + 1))

grid_res  <- 60
x1_grid   <- seq(min(df$x1), max(df$x1), length.out = grid_res)
x2_grid   <- seq(min(df$x2), max(df$x2), length.out = grid_res)
z_mat     <- outer(x1_grid, x2_grid, f_hat)         # rows: x1, cols: x2
z_min     <- min(z_mat)
y_margin  <- min(x2_grid)                           # back wall (x2 axis)

## 3  COLOUR MAP  (60 % α) ----------------------------------------------------
raw_cols  <- c("#4575b4", "#91bfdb", "#e0f3f8",
  "#ffffbf", "#fee090", "#fc8d59", "#d73027")
pal_fun   <- colorRampPalette(raw_cols)
surface_cols <- pal_fun(100)
surface_cols <- grDevices::adjustcolor(surface_cols, alpha.f = 0.60)
plotly_pal   <- Map(list,
  seq(0, 1, length.out = 100),
  surface_cols)

## 4  STEP-SPECIFIC DATA ------------------------------------------------------
### 4.1  finite differences in first interval (j = 1)
j      <- 1
z_low  <- breaks_x1[j]
z_high <- breaks_x1[j + 1]
df_sub <- df %>%
  filter(x1 >= z_low, x1 < z_high) %>%
  mutate(x1_low  = z_low,
    x1_high = z_high,
    y_low   = f_hat(x1_low,  x2),
    y_high  = f_hat(x1_high, x2))

delta_bar <- mean(df_sub$y_high - df_sub$y_low)
x2_mid    <- median(df_sub$x2)                 # for red average arrow

### 4.2  ALE accumulation (uncentred here; add centring if desired)
ale_tbl <- purrr::map_dfr(seq_len(K), function(j) {
  id   <- df$x1 >= breaks_x1[j] & df$x1 < breaks_x1[j + 1]
  delt <- with(df[id, ],
    f_hat(breaks_x1[j + 1], x2) -
      f_hat(breaks_x1[j],     x2))
  tibble(j = j,
    z_low  = breaks_x1[j],
    z_high = breaks_x1[j + 1],
    delta_bar = mean(delt))
}) %>%
  mutate(ale   = cumsum(delta_bar),
    z_mid = 0.5 * (z_low + z_high))
# Optional centring:
# ale_tbl$ale <- ale_tbl$ale - mean(ale_tbl$ale)

## 5  SHARED BASE PLOT  (surface, points, interval guides) -------------------
base_plot <- plot_ly(
  x = x1_grid,                     # x-axis = x1
  y = x2_grid,                     # y-axis = x2
  z = t(z_mat),                    # transpose → rows = y, cols = x
  type = "surface",
  opacity = 0.60,
  colorscale = plotly_pal,
  showscale  = FALSE
) |>
  layout(scene = list(
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2"),
    zaxis = list(title = "f\u0302"),
    camera = list(eye = as.list(c(x = -0.1, y = -1.3, z = 2)*1)))
  )

## add observed points
base_plot <- base_plot |>
  add_markers(data = df,
    x = ~x1, y = ~x2, z = ~yhat,
    marker = list(color = "black", size = 3),
    showlegend = FALSE)
base_plot

## add interval guides (surface ridge, back-wall line, base line)
for (b in breaks_x1) {
  # ridge on surface (constant x1 = b)
  base_plot <- base_plot |>
    add_trace(x = rep(b, grid_res), y = x2_grid,
      z = f_hat(b, x2_grid),
      type = "scatter3d", mode = "lines",
      line = list(color = "grey", width = 4),
      showlegend = FALSE)
  # back-wall margin (x2 = y_margin)
  base_plot <- base_plot |>
    add_trace(x = b, y = y_margin,
      z = c(z_min, f_hat(b, y_margin)),
      type = "scatter3d", mode = "lines",
      line = list(color = "grey", width = 4),
      showlegend = FALSE)
  # base-plane line (z = z_min)
  base_plot <- base_plot |>
    add_trace(x = b, y = c(min(x2_grid), max(x2_grid)),
      z = z_min,
      type = "scatter3d", mode = "lines",
      line = list(color = "grey", width = 4),
      showlegend = FALSE)
}

## 6  STEP 1 – just the base ---------------------------------------------------
p1 <- base_plot
plotly::save_image(p1, "../figure/ale_step1_3D_plotly.pdf", width = 800, height = 600)

## 7  STEP 2 – add blue finite-difference segments ----------------------------
p2 <- base_plot
for (k in seq_len(nrow(df_sub))) {
  p2 <- p2 |>
    add_trace(
      x = c(df_sub$x1_low[k],  df_sub$x1_high[k]),
      y = rep(df_sub$x2[k], 2),
      z = c(df_sub$y_low[k],  df_sub$y_high[k]),
      type = "scatter3d", mode = "lines",
      line = list(color = "blue", width = 6),
      showlegend = FALSE)
}
plotly::save_image(p2, "../figure/ale_step2_3D_plotly.pdf", width = 800, height = 600)

## 8  STEP 3 – add red average finite-difference arrow ------------------------
p3 <- base_plot |>
  add_trace(
    x = c(z_low, z_high),
    y = rep(x2_mid, 2),
    z = c(f_hat(z_low,  x2_mid),
      f_hat(z_high, x2_mid)),
    type = "scatter3d", mode = "lines",
    line = list(color = "red", width = 8),
    showlegend = FALSE)
plotly::save_image(p3, "../figure/ale_step3_3D_plotly.pdf", width = 800, height = 600)

## 9  STEP 4 – add red ALE ridge ---------------------------------------------
p4 <- base_plot
for (k in seq_len(nrow(ale_tbl))) {
  p4 <- p4 |>
    add_trace(
      x = c(ale_tbl$z_low[k], ale_tbl$z_high[k]),
      y = rep(y_margin, 2),
      z = rep(ale_tbl$ale[k], 2),
      type = "scatter3d", mode = "lines",
      line = list(color = "red", width = 6),
      showlegend = FALSE)
}
plotly::save_image(p4, "../figure/ale_step4_3D_plotly.pdf", width = 800, height = 600)

###############################################################################
# END  – four vector PDFs now in your working directory                       #
###############################################################################













# 
# 
# 
# ###############################################################################
# # STATIC 3-D PLOTS (PDF) – surface & margin interval lines added              #
# ###############################################################################
# library(plot3D)
# 
# # colour map with 60 % transparency
# pal <- colorRampPalette(
#   c("#4575b4", "#91bfdb", "#e0f3f8",
#     "#ffffbf", "#fee090", "#fc8d59", "#d73027"))(100)
# 
# y_margin  <- min(x2_grid)          # back wall (x1 ×  f̂  plane)
# z_surface <- function(x) f_hat(x, y_margin)
# 
# make_static3d <- function(file, extra_layers) {
#   pdf(file, width = 6, height = 5, useDingbats = FALSE)
#   on.exit(dev.off())
#   
#   ## base surface -------------------------------------------------------------
#   plist <<- persp3D(
#     x = x1_grid, y = x2_grid, z = z_mat,
#     colvar = z_mat, col = pal, alpha = 0.6,
#     colkey = FALSE, ticktype = "detailed",
#     xlab = "x1", ylab = "x2", zlab = expression(hat(f)),
#     main = sub("_3D\\.pdf$", "", basename(file))
#   )
#   
#   ## observed points ----------------------------------------------------------
#   points3D(df$x1, df$x2, df$yhat, pch = 16, cex = 0.6,
#     col = "black", add = TRUE, plist = plist)
#   
#   ## interval structure -------------------------------------------------------
#   for (b in breaks_x1) {
#     
#     ## 1) plane-base line (already present, kept for reference)
#     segments3D(b, min(x2_grid), min(z_mat),
#       b, max(x2_grid), min(z_mat),
#       col = "darkgrey", lwd = 1.5, add = TRUE, plist = plist)
#     
#     ## 2) surface ridge
#     lines3D(rep(b, grid_res), x2_grid,
#       f_hat(b, x2_grid), col = "darkgrey",
#       lwd = 1.5, add = TRUE, plist = plist)
#     
#     ## 3) margin (x1 × f̂) line on back wall
#     segments3D(b, y_margin, min(z_mat),
#       b, y_margin, z_surface(b),
#       col = "darkgrey", lwd = 1.5, add = TRUE, plist = plist)
#   }
#   
#   ## step-specific graphics ---------------------------------------------------
#   extra_layers()
# }
# 
# ###############################################################################
# # STEP-wise calls                                                             #
# ###############################################################################
# make_static3d("../figure/ale_step1_3D.pdf", function() {})
# 
# make_static3d("../figure/ale_step2_3D.pdf", function() {
#   apply(df_sub, 1L, function(r)
#     arrows3D(r["x1_low"],  r["x2"], r["y_low"],
#       r["x1_high"], r["x2"], r["y_high"],
#       col = "blue", lwd = 2, add = TRUE, plist = plist))
# })
# 
# make_static3d("../figure/ale_step3_3D.pdf", function() {
#   arrows3D(z_low,  x2_mid, f_hat(z_low,  x2_mid),
#     z_high, x2_mid, f_hat(z_high, x2_mid),
#     col = "red", lwd = 3, add = TRUE, plist = plist)
# })
# 
# make_static3d("../figure/ale_step4_3D.pdf", function() {
#   for (k in seq_len(nrow(ale_tbl)))
#     segments3D(ale_tbl$z_low[k],  y_margin, ale_tbl$ale[k],
#       ale_tbl$z_high[k], y_margin, ale_tbl$ale[k],
#       col = "red", lwd = 2, add = TRUE, plist = plist)
# })
# ###############################################################################
# # END                                                                         #
# ###############################################################################
# 





