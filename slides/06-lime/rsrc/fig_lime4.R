# PREREQ -----------------------------------------------------------------------
source("fig_lime3.R")

# PLOT -----------------------------------------------------------------------
xorig <- c(2.8, 3.5)
euclidean <- function(x) sqrt(sum((xorig - x)^2))
kernel <- function(x, kernel.width = 0.1) {
  d = dist(rbind(x, xorig), method = "euclidean")
  sqrt(exp(-(d^2) / (kernel.width^2)))
}

size <- apply(data_train, 1, kernel, kernel.width = 1)
# range01 <- function(x){0.5*(x-min(x))/(max(x)-min(x))}
# size <- range01(size)

# add weights
fig0 <- fig0 + geom_point(aes(x = x1train, y = x2train, size = size), col = "blue") + 
  theme(legend.title = element_blank(), legend.position = "none")
fig0
ggsave("../figure/lime4.pdf", plot = fig0, width = 4, height = 3)
