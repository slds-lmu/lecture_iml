# PREREQ -----------------------------------------------------------------------
source("fig_lime1.R")

# PLOT -----------------------------------------------------------------------
x1train <-  runif(200, 0, 10)
x2train <-  runif(200, 0, 8)
data_train <- data.frame(x1 = x1train, x2 = x2train)

# add point to explain
fig0 <- fig0 + geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig0 
ggsave("../figure/lime2.pdf", plot = fig0, width = 4, height = 3)