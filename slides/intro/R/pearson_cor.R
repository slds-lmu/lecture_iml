# PREREQ -----------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

set.seed(123)
load("data/bike.RData")

#Rectangles that enter the covariance with positive sign.

mean.temp = mean(bike$temp)
mean.cnt = mean(bike$cnt)

p.base = ggplot(bike) + geom_point(aes(x = temp, y = cnt)) + #, alpha = .5) +
  geom_hline(yintercept = mean.cnt, linetype = "dashed") +
  geom_vline(xintercept = mean.temp, linetype = "dashed") +
  geom_text(aes(x = mean((temp)), label = "bar(x)", y = max(cnt)), parse = T, hjust = 1) +
  geom_text(aes(x = min((temp)), label = "bar(y)", y = mean(cnt)), parse = T, vjust = -0.75) +
  theme(axis.title = element_text(size = 14),
    plot.title = element_text(size = rel(4)))

ind = which(bike$temp > quantile(bike$temp, 0.9) & bike$cnt > quantile(bike$cnt, 0.9))[1]
ind.temp = bike$temp[ind]
ind.cnt = bike$cnt[ind]
p = p.base + geom_point(aes(x = ind.temp, y = ind.cnt), color = "red", size = 4) +
  geom_rect(data = bike[ind,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "red", fill = "red") +
  annotate("text", x = mean(c(ind.temp, mean.temp)),
    y = ind.cnt,
    label = "x[i] - bar(x)", parse = T, colour = "red", vjust = -0.2) +
  annotate("text", x = ind.temp,
    y = mean(c(ind.cnt, mean.cnt)),
    label = "y[i] - bar(y)", parse = T, colour = "red", hjust = -0.2)

ind2 = which(bike$temp < quantile(bike$temp, 0.1) & bike$cnt < quantile(bike$cnt, 0.1))[1]
ind.temp2 = bike$temp[ind2]
ind.cnt2 = bike$cnt[ind2]
p + geom_point(x = ind.temp2, y = ind.cnt2,
  color = "red", size = 4) +
  geom_rect(data = bike[ind2,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "red", fill = "red") +
  annotate("text", x = mean(c(ind.temp2, mean.temp)),
    y = ind.cnt2,
    label = "x[i] - bar(x)", parse = T, colour = "red", vjust = 1.2) +
  annotate("text", x = ind.temp2,
    y = mean(c(ind.cnt2, mean.cnt)),
    label = "y[i] - bar(y)", parse = T, colour = "red", hjust = 1.2)

## Rectangles that enter the covariance with negative sign.

ind = which(bike$temp > quantile(bike$temp, 0.7) & bike$cnt < quantile(bike$cnt, 0.3))[1]
ind.temp = bike$temp[ind]
ind.cnt = bike$cnt[ind]
p = p.base + geom_point(aes(x = ind.temp, y = ind.cnt),
  color = "blue", size = 4) +
  geom_rect(data = bike[ind,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "blue", fill = "blue") +
  annotate("text", x = mean(c(ind.temp, mean.temp)),
    y = ind.cnt,
    label = "x[i] - bar(x)", parse = T, colour = "blue", vjust = 1.2) +
  annotate("text", x = ind.temp,
    y = mean(c(ind.cnt, mean.cnt)),
    label = "y[i] - bar(y)", parse = T, colour = "blue", hjust = -0.2)

ind2 = which(bike$temp < quantile(bike$temp, 0.3) & bike$cnt > quantile(bike$cnt, 0.7))[1]
ind.temp2 = bike$temp[ind2]
ind.cnt2 = bike$cnt[ind2]
p + geom_point(x = ind.temp2, y = ind.cnt2,
  color = "blue", size = 4) +
  geom_rect(data = bike[ind2,], aes(xmin = mean.temp, ymin = mean.cnt, xmax = temp, ymax = cnt), alpha = .1, color = "blue", fill = "blue") +
  annotate("text", x = mean(c(ind.temp2, mean.temp)),
    y = ind.cnt2,
    label = "x[i] - bar(x)", parse = T, colour = "blue", vjust = -0.2) +
  annotate("text", x = ind.temp2,
    y = mean(c(ind.cnt2, mean.cnt)),
    label = "y[i] - bar(y)", parse = T, colour = "blue", hjust = 1.2)
