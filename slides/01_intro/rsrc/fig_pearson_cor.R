# PREREQ -----------------------------------------------------------------------
library(patchwork)
library(ggplot2)
library(ggpubr)
theme_set(theme_bw() + theme(plot.margin=grid::unit(c(1,5.5,1,1), "pt")))

# DATA -------------------------------------------------------------------------

negcol = "#F8766D"
poscol = "#00BFC4"
set.seed(123)
load("../../../data/bike.RData")
mean.temp = mean(bike$temp)
mean.cnt = mean(bike$cnt)

# #Rectangles that enter the covariance with positive sign.
#
#
# p.base = ggplot(bike) + geom_point(aes(x = temp, y = cnt), alpha = .5) +
#   geom_hline(yintercept = mean.cnt, linetype = "dashed") +
#   geom_vline(xintercept = mean.temp, linetype = "dashed") +
#   geom_text(aes(x = mean((temp)), label = "bar(x)", y = max(cnt)), parse = T, hjust = 1) +
#   geom_text(aes(x = min((temp)), label = "bar(y)", y = mean(cnt)), parse = T, vjust = -0.75) +
#   theme(axis.title = element_text(size = 14),
#     plot.title = element_text(size = rel(4)))
#
# ind = which(bike$temp > quantile(bike$temp, 0.9) & bike$cnt > quantile(bike$cnt, 0.9))[1]
# ind.temp = bike$temp[ind]
# ind.cnt = bike$cnt[ind]
# p = p.base +
#   geom_rect(data = bike[ind,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "black", fill = poscol) +
#   geom_point(aes(x = ind.temp, y = ind.cnt), color = poscol, size = 4) +
#   annotate("label", x = mean(c(ind.temp, mean.temp)),
#     y = ind.cnt,
#     label = "x[i] - bar(x)", parse = T, colour = poscol, vjust = -0.2) +
#   annotate("label", x = ind.temp,
#     y = mean(c(ind.cnt, mean.cnt)),
#     label = "y[i] - bar(y)", parse = T, colour = poscol, hjust = -0.2)
#
# ind2 = which(bike$temp < quantile(bike$temp, 0.1) & bike$cnt < quantile(bike$cnt, 0.1))[1]
# #ind.temp2 = bike$temp[ind2]
# #ind.cnt2 = bike$cnt[ind2]
# p1 = p +
#   geom_rect(data = bike[ind2,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "black", fill = poscol) +
#   geom_point(x = bike$temp[ind2], y = bike$cnt[ind2],
#     color = poscol, size = 4) +
#   annotate("label", x = mean(c(bike$temp[ind2], mean.temp)),
#     y = bike$cnt[ind2],
#     label = "x[i] - bar(x)", parse = T, colour = poscol, vjust = 1.2) +
#   annotate("label", x = bike$temp[ind2],
#     y = mean(c(bike$cnt[ind2], mean.cnt)),
#     label = "y[i] - bar(y)", parse = T, colour = poscol, hjust = 1.2)
#
# ## Rectangles that enter the covariance with negative sign.
#
# ind = which(bike$temp > quantile(bike$temp, 0.7) & bike$cnt < quantile(bike$cnt, 0.2))[1]
# #ind.temp = bike$temp[ind]
# #ind.cnt = bike$cnt[ind]
#
# p = p.base +
#   geom_rect(data = bike[ind,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "black", fill = negcol) +
#   geom_point(aes(x = bike$temp[ind], y = bike$cnt[ind]), color = negcol, size = 4) +
#   annotate("label", x = mean(c(bike$temp[ind], mean.temp)),
#     y = bike$cnt[ind],
#     label = "x[i] - bar(x)", parse = T, colour = negcol, vjust = 1.2) +
#   annotate("label", x = bike$temp[ind],
#     y = mean(c(bike$cnt[ind], mean.cnt)),
#     label = "y[i] - bar(y)", parse = T, colour = negcol, hjust = -0.2)
#
# ind2 = which(bike$temp < quantile(bike$temp, 0.3) & bike$cnt > quantile(bike$cnt, 0.7))[1]
# #ind.temp2 = bike$temp[ind2]
# #ind.cnt2 = bike$cnt[ind2]
# p2 = p + geom_rect(data = bike[ind2,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "black", fill = negcol) +
#   geom_point(x = bike$temp[ind2], y = bike$cnt[ind2],
#     color = negcol, size = 4) +
#   annotate("label", x = mean(c(bike$temp[ind2], mean.temp)),
#     y = bike$cnt[ind2],
#     label = "x[i] - bar(x)", parse = T, colour = negcol, vjust = -0.2) +
#   annotate("label", x = bike$temp[ind2],
#     y = mean(c(bike$cnt[ind2], mean.cnt)),
#     label = "y[i] - bar(y)", parse = T, colour = negcol, hjust = 1.2)
#
# p1 = p1 + ggtitle("Rectangles with positive area") +
#   theme(plot.title = element_text(size = 16))
# p2 = p2 + ggtitle("Rectangles with negative area") +
#   theme(plot.title = element_text(size = 16))


bike$Rectangle_Area = ifelse(
  (bike$temp > mean(bike$temp) & bike$cnt > mean(bike$cnt)) |
  (bike$temp < mean(bike$temp) & bike$cnt < mean(bike$cnt)),
  "Positive", "Negative")

area = (bike$temp-mean(bike$temp))*(bike$cnt-mean(bike$cnt))

ind.pos1 = which(bike$temp > quantile(bike$temp, 0.9) & bike$cnt > quantile(bike$cnt, 0.9))[1]
ind.pos2 = which(bike$temp < quantile(bike$temp, 0.1) & bike$cnt < quantile(bike$cnt, 0.1))[1]

ind.neg1 = which(bike$temp > quantile(bike$temp, 0.7) & bike$cnt < quantile(bike$cnt, 0.2))[1]
ind.neg2 = which(bike$temp < quantile(bike$temp, 0.3) & bike$cnt > quantile(bike$cnt, 0.7))[1]

ind = c(ind.pos1, ind.pos2, ind.neg1, ind.neg2)

xlab = c("x[1]^{(i)} - bar(x)[1] > 0", "x[1]^{(i)} - bar(x)[1] < 0", "x[1]^{(i)} - bar(x)[1] > 0", "x[1]^{(i)} - bar(x)[1] < 0")
ylab = c("x[2]^{(i)} - bar(x)[2] > 0", "x[2]^{(i)} - bar(x)[2] < 0", "x[2]^{(i)} - bar(x)[2] < 0", "x[2]^{(i)} - bar(x)[2] > 0")

plot = ggplot(bike, aes(x = temp, y = cnt)) +
  geom_rect(data = bike[ind,], aes(xmax = temp, ymax = cnt, fill = Rectangle_Area),
    xmin = mean(bike$temp), ymin = mean(bike$cnt),
    alpha = 0.5, colour = "black") +
  geom_hline(yintercept = mean.cnt, linetype = "dashed", colour = "blue", lwd = 2) +
  geom_vline(xintercept = mean.temp, linetype = "dashed", colour = "blue", lwd = 2) +
  geom_text(aes(x = mean((temp)), label = "bar(x)[1]", y = max(cnt)), parse = T, hjust = 1.5, colour = "blue") +
  geom_text(aes(x = min((temp)), label = "bar(x)[2]", y = mean(cnt)), parse = T, vjust = -0.75, colour = "blue") +
  geom_point(aes(x = temp, y = cnt)) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, cor.coef.name = expression("Person's "~rho), aes(label = ..r.label..)) +
  geom_point(data = bike[ind,], aes(x = temp, y = cnt, fill = Rectangle_Area), size = 4, col = "black", pch = 21) +
  # geom_rect(aes(xmax = temp[ind.pos1], ymax = cnt[ind.pos1]),
  #   xmin = mean(bike$temp), ymin = mean(bike$cnt),
  #   alpha = 0, colour = "blue", lty = 2, lwd = 2) +
  # geom_rect(aes(xmax = temp[ind.neg1], ymax = cnt[ind.neg1]),
  #   xmin = mean(bike$temp), ymin = mean(bike$cnt),
  #   alpha = 0, colour = "blue", lty = 2, lwd = 2) +
    annotate("label", x = (bike$temp[ind] + mean.temp)/2,
      y = bike$cnt[ind],
      label = xlab, parse = T, colour = "black", vjust = 0.1) +
  annotate("label", x = bike$temp[ind],
      y = (bike$cnt[ind] + mean.cnt)/2,
      label = ylab, parse = T, colour = "black", hjust = 0.9) +
  geom_point(x = mean(bike$temp), y = mean(bike$cnt), colour = "blue") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = expression(X[1]~": Temperature in °C"), y = expression(X[2]~": Number of bike rentals"))

ggsave(filename = "../figure/pearson_cor.pdf", plot,
  width = 9, height = 5.5)
