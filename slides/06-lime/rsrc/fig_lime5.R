# PREREQ -----------------------------------------------------------------------
source("fig_lime4.R")

# PLOT -----------------------------------------------------------------------
# add linear model 
fig0 <- fig0 + geom_abline(intercept = 1.85, slope = 0.58, color="red", 
                           linetype="dashed", size=1.51) +
  geom_point(aes(x = x1train, y = x2train), col = "blue") +
  theme(legend.title = element_blank(), legend.position = "none")  
fig0 <- fig0 + geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig0
ggsave("../figure/lime5.pdf", plot = fig0, width = 4, height = 3)