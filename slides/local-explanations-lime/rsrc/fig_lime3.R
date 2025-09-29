# PREREQ -----------------------------------------------------------------------
source("fig_lime2.R")

# PLOT -----------------------------------------------------------------------
# add sampled points
fig0 <- fig0 + geom_point(aes(x = x1train, y = x2train), col = "blue")
fig0
ggsave("../figure/lime3.pdf", plot = fig0, width = 4, height = 3)
