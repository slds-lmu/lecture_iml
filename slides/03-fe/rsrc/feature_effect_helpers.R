# HELPER -----------------------------------------------------------------------

shadowtext = function(x, y = NULL, labels, col = 'black', bg = 'white',
                      theta = seq(0, 2*pi, length.out = 50), r = 0.1, ... ) {
  
  xy = xy.coords(x,y)
  xo = r*strwidth('A')
  yo = r*strheight('A')
  
  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col = bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col = col, ... )
}

plotImportanceDemo = function(x, dL, split, i = 1, ylab = expression(Delta~L), main = "Individual Conditional Importance (ICI) Curves", col = "black", polygon = FALSE) {
  plot(NULL, xlim = c(1, 3), ylim = c(0, 1), type = "n",
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       frame.plot = FALSE, # Remove the frame
       panel.first = {
         grid(lty = 1)
         if (polygon) polygon(c(split[[i]]$x, rev(split[[i]]$x)), c(split[[i]]$dL, rep(0, length(split[[i]]$x))), border = NA, col = rgb(0, 0, 0, 0.25))
       })
  box(bty = "L")
  title(main = main, adj = 0, font.main = 1)
  title(xlab = expression(x[1]), ylab = ylab, line = 2)
  axis(1, at = c(1, 2, 3), padj = -0.5, cex.axis = 0.8, col.axis = "gray40")
  axis(2, at = seq(0, 1, by = 0.2), las = 2, hadj = 0.75,
       labels = prettyNum(seq(0, 1, by = 0.2)), padj = 0.5, cex.axis = 0.8, col.axis = "gray40")
  
  for (d in split[1:i]) {
    lines(d$x, d$dL, lty = d$pch + 1, lwd = 2, col = col)
    points(d$x, d$dL, pch = 19, col = col)
    shadowtext(d$x, d$dL - 0.025,
               labels = paste0("i=", d$pch), pos = 3, col = col)
  }
}

# DATA -------------------------------------------------------------------------

i = pch = rep(1:3, 3)
x = c(1,1,1,2,2,2,3,3,3)
y = c(
  1,1,0,
  1,1,0,
  1,1,0)
fh = c(
  0.7, 0.7, 0.1,
  0.4, 0.7, 0.1,
  0.7, 0.6, 0.3)
l = abs(y - fh)
li = l[c(1,5,9)]
li = li[i]
dL = c(0.4,0.6,0.1,0.6,0.8,0.5,0.7,0.9,0.6)
lreplace = dL + mean(l)
X = data.frame(pch, x, dL)
X.aggr = aggregate(dL ~ x, data = X, FUN = mean, na.rm = TRUE)

split = split(X, X$pch)
split = lapply(split, function(x) x[order(x$x), ])