# PREREQ -----------------------------------------------------------------------
library(ggplot2)
library(xtable)
library(entropy)

# DATA -------------------------------------------------------------------------
set.seed(1234L)
x1 = round(seq(-1, 1, 0.25), 2)
x2 = round(x1^2+rnorm(length(x1), sd = 0.04), 2)
y = round(5*x1 + -2*x2 + rnorm(length(x1)), 2)
d = data.frame(y, x1, x2)

# calculate histogram and take mean for each interval
pdf(file = "exercises/01_introduction/figure/hist_x1_x2.pdf", 
    width = 6, height = 4)
par(mfrow = c(1,2))
h1 = hist(x1, freq = FALSE, include.lowest = TRUE, breaks = 3)
h2 = hist(x2, freq = FALSE, include.lowest = TRUE, breaks = 3)
dev.off()

x1_d = numeric()
for(i in 1:length(x1)){
  vec = logical()
  for(j in 1:(length(h1$breaks)-1)) vec[j] = x1[i] > h1$breaks[j] & x1[i] <= h1$breaks[j+1]
  if(x1[i] == h1$breaks[1]) vec[1] = 1
  x1_d[i] = h1$mids[vec]
}
x2_d = numeric()
for(i in 1:length(x2)){
  vec = logical()
  for(j in 1:(length(h2$breaks)-1)) vec[j] = x2[i] > h2$breaks[j] & x2[i] <= h2$breaks[j+1]
  if(x2[i] == h2$breaks[1]) vec[1] = 1
  x2_d[i] = h2$mids[vec]
}
xtable(t(data.frame(x1_d, x2_d)))

# joint frequency matrix
tab = prop.table(table(data.frame(x1_d, x2_d)))
tabx = cbind(tab, rowSums(tab))
tabx = rbind(tabx, colSums(tabx))
xtable(tabx)

# calculate MI
0.11 * log(0.11 / (0.33 * 0.44)) + 0.22 * log(0.22 / (0.33 * 0.44)) + 
  0.11 * log(0.11 / (0.22 * 0.11)) + 0.11 * log(0.11 / (0.22 * 0.44)) + 
  0.22 * log(0.22 / (0.22 * 0.44)) + 0.22 * log(0.22 / (0.22 * 0.44)) 
# without rounded values
mi.plugin(tab)

# PLOT -------------------------------------------------------------------------
ggplot(d, aes(x = x1, y = x2)) +
  geom_point() +
  theme_bw()

ggsave("exercises/01_introduction/figure/add_Points_x1_x2_sol.pdf", width = 3, height = 2)
