# PREREQ -----------------------------------------------------------------------
source("fig_lime2.R")

# PLOT -----------------------------------------------------------------------
#----------- SAMPLING STRATEGIES ----------------------
fig0 <- ggplot(data, aes(x = x, y = y)) + 
  # geom_smooth(se = FALSE, col = "black") + 
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(y ~ x))),
              alpha = 0.6,fill = 'lightgray') + 
  geom_ribbon(aes(ymin = predict(loess(y ~ x)), ymax = 7),
              alpha = 0.9,fill = 'darkgray') + 
  theme_bw() + ylim(c(0, 7.4)) +
  ylab("x2") + xlab("x1") 
fig0

x1train <-  seq(0.5, 9.5, length.out = 14)
x2train <-  seq(0.3, 8, length.out = 15)
data_train <- data.frame(expand.grid(x1train, x2train))

# GRID
fig1 <- fig0 + geom_point(aes(x = data_train$Var1[1:200], 
                              y = data_train$Var2[1:200]), col = "blue") + 
  ylim(c(1, 7.5)) +
  ylim(c(0, 7)) +
  geom_point(aes(x=2.8, y=3.5), colour="yellow", cex = 3)
fig1
ggsave("../figure/lime3a.pdf", plot = fig1, width = 4, height = 3)
