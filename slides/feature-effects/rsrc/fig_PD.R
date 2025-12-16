# PREREQ -----------------------------------------------------------------------

source("feature_effect_helpers.R")

# PLOT -------------------------------------------------------------------------

pdf(file = "../figure/PD.pdf", width = 5.5, height = 3.75)

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 3, col = "gray")

splitx = split(X, X$x)
i = 1
d = splitx[[i]]
col = "red"
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[i], X.aggr$dL[i], type = "b", lwd = 3, col = col)

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 3, col = "gray")

d = splitx[[2]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x[1:2], X.aggr$dL[1:2], type = "b", lwd = 3, col = col)

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 3, col = "gray")

d = splitx[[3]]
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
lines(X.aggr$x, X.aggr$dL, type = "b", lwd = 3, col = col)

dev.off()
