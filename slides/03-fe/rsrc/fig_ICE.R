# PREREQ -----------------------------------------------------------------------

source("feature_effect_helpers.R")

# PLOT -------------------------------------------------------------------------

pdf(file = "../figure/ICE.pdf", width = 5.5, height = 3.75)
pch.sym = paste0("i=", c("1","2","3"))
p = pch.sym[pch]
#p[p == x] = NA

split = split(X, X$pch)
split = lapply(split, function(x) x[order(x$x), ])


par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x[1], dL[1], ylab = expression(hat(f)[S]), main = "",
                   split = lapply(split[1], function(x) x[1,]), i = 1, col  = "red")

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x[1], dL[1], ylab = expression(hat(f)[S]), main = "",
                   split = lapply(split[1], function(x) x[1:2,]), i = 1, col  = "red")

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 1, col  = "red")

######################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 1)

d = split[[2]]
col = "red"
lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
#########################################################

par(mar = c(3,3.5,0.25,0.25))
plotImportanceDemo(x, dL, ylab = expression(hat(f)[S]), main = "",
                   split = split, i = 2)

d = split[[3]]
col = "red"
lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
points(d$x, d$dL, pch = 19, col = col)
shadowtext(d$x, d$dL - 0.025,
           labels = paste0("i=", d$pch), pos = 3, col = col)
dev.off()
