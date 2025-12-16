# PREREQ -----------------------------------------------------------------------

source(textConnection(
  readLines("fig_ale_maineff.R")[c(1:32, 36:42)]
))

# PLOT -------------------------------------------------------------------------

pdf("../figure/ale_maineffmaunal.pdf")

par(mfrow = c(2, 2), mar = c(4,4,2,2) + 0.1)
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1",
     ylab="ALE_main_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(a)")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(PD.1$x.values, PD.1$f.values, type="l", xlab="x1",
     ylab="PD_x1", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(b) ")
curve(x - 0.5, from = 0, to = 1, add = TRUE)
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x2",
     ylab="ALE_main_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(c) ")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)
plot(PD.2$x.values, PD.2$f.values, type="l", xlab="x2",
     ylab="PD_x2", xlim = c(0,1), ylim = c(-1,1), col = "blue", main = "(d) ")
curve(x^2 - (1/3+0.05^2), from = 0, to = 1, add = TRUE)

dev.off()