library(plot3D)
shadowtext = function(x, y = NULL, labels, col = 'black', bg = 'white',
  theta = seq(0, 2*pi, length.out = 50), r = 0.25, ... ) {
  if (length(x) == 1 & length(y) != 1) {
    x = x[rep(1, length(y))]
  }
  if (length(y) == 1 & length(x) != 1) {
    y = y[rep(1, length(x))]
  }
  xy = xy.coords(x,y)
  xo = r*strwidth('A')
  yo = r*strheight('A')
  # draw background text with smawll shift in x and y in background colour
  for (i in theta) {
    text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col = bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col = col, ... )
}



pdf("../figure/interaction_separable.pdf", width = 8.25, height = 5.5)
pred = function(x, y) {
  #(x-10)^2 - (y-10)^2 + (x*y)
  x + y + x*y
}
grid.lines = 20
x.pred = seq(1, 10, length.out = grid.lines)
y.pred = seq(1, 10, length.out = grid.lines)
slices = c(2, 5, 9)
xy = expand.grid(x = x.pred, y = y.pred)


line.df = data.frame(
  x = seq(min(x.pred), max(x.pred), length = 10),
  y = seq(min(y.pred), max(y.pred), length = 10)
)


layout(matrix(c(1,1,1,2,2,3,3,4,4,4,5,5,6,6), nrow = 2, byrow = TRUE))
z.pred = matrix(pred(xy$x, xy$y), nrow = grid.lines, ncol = grid.lines)

par(mar = c(2,2,1,1))
p = persp3D(x.pred, y.pred, z.pred, ticktype = "detailed",
  theta = -30, phi = 20, lighting = TRUE, lphi = 40,
  xlab = "x1", ylab = "x2", zlab = "f(x)",
  bty = "b2", col = terrain.colors(100), #border = "#FFFFFF11",
  main = expression("f(x) = "~x[1]+x[2]+x[1]*x[2]))

for(i in 1:length(slices)) {
  lines(trans3D(x = slices[i], y = line.df$y,
    z = pred(slices[i], line.df$y), pmat = p),
    lwd = 2, lty = i, col = "blue")
  lines(trans3D(x = line.df$x, y = slices[i],
    z = pred(line.df$x, slices[i]), pmat = p),
    lwd = 2, lty = i, col = "black")
}

par(mar = c(4,5,2,0.1))
plot(line.df$x, pred(line.df$x, slices[1]),
  type = "l", ylim = range(z.pred), lty = 1, lwd = 2,
  xlab = expression(x[1]), ylab = "f(x)")
lines(line.df$x, pred(line.df$x, slices[2]), lty = 2, lwd = 2)
lines(line.df$x, pred(line.df$x, slices[3]), lty = 3, lwd = 2)
lab = c(
  as.expression(bquote(x[2]~" = "~.(slices[1]))),
  as.expression(bquote(x[2]~" = "~.(slices[2]))),
  as.expression(bquote(x[2]~" = "~.(slices[3]))))
shadowtext(x = 6, y = pred(6, slices), lab, pos = 1)


plot(line.df$y, pred(slices[1], line.df$y), type = "l",
  ylim = range(z.pred), lty = 1, lwd = 2,
  xlab = expression(x[2]), ylab = "f(x)", col = "blue")
lines(line.df$y, pred(slices[2], line.df$y), col = "blue", lty = 2, lwd = 2)
lines(line.df$y, pred(slices[3], line.df$y), col = "blue", lty = 3, lwd = 2)
lab = c(
  as.expression(bquote(x[1]~" = "~.(slices[1]))),
  as.expression(bquote(x[1]~" = "~.(slices[2]))),
  as.expression(bquote(x[1]~" = "~.(slices[3]))))
shadowtext(x = 6, y = pred(6, slices), lab, pos = 1, col = "blue")


pred = function(x, y) {
  #(x-10)^2 - (y-10)^2 + (x*y)
  x + y + log(x*y)
}
z.pred = matrix(pred(xy$x, xy$y), nrow = grid.lines, ncol = grid.lines)

par(mar = c(2,2,1,1))
p = persp3D(x.pred, y.pred, z.pred, ticktype = "detailed",
  theta = -30, phi = 20, lighting = TRUE, lphi = 40,
  xlab = "x1", ylab = "x2", zlab = "f(x)",
  bty = "b2", col = terrain.colors(100), #border = "#FFFFFF11",
  main = expression("f(x) = "~x[1]+x[2]+log(x[1]*x[2])))

for(i in 1:length(slices)) {
  lines(trans3D(x = slices[i], y = line.df$y,
    z = pred(slices[i], line.df$y), pmat = p),
    lwd = 2, lty = i, col = "blue")
  lines(trans3D(x = line.df$x, y = slices[i],
    z = pred(line.df$x, slices[i]), pmat = p),
    lwd = 2, lty = i, col = "black")
}


par(mar = c(4,5,2,0.1))
plot(line.df$x, pred(line.df$x, slices[1]),
  type = "l", ylim = range(z.pred), lty = 1, lwd = 2,
  xlab = expression(x[1]), ylab = "f(x)")
lines(line.df$x, pred(line.df$x, slices[2]), lty = 2, lwd = 2)
lines(line.df$x, pred(line.df$x, slices[3]), lty = 3, lwd = 2)
lab = c(
  as.expression(bquote(x[2]~" = "~.(slices[1]))),
  as.expression(bquote(x[2]~" = "~.(slices[2]))),
  as.expression(bquote(x[2]~" = "~.(slices[3]))))
shadowtext(x = 6, y = pred(6, slices), lab, pos = 1)



plot(line.df$y, pred(slices[1], line.df$y), type = "l",
  ylim = range(z.pred), lty = 1, lwd = 2,
  xlab = expression(x[2]), ylab = "f(x)", col = "blue")
lines(line.df$y, pred(slices[2], line.df$y), col = "blue", lty = 2, lwd = 2)
lines(line.df$y, pred(slices[3], line.df$y), col = "blue", lty = 3, lwd = 2)
lab = c(
  as.expression(bquote(x[1]~" = "~.(slices[1]))),
  as.expression(bquote(x[1]~" = "~.(slices[2]))),
  as.expression(bquote(x[1]~" = "~.(slices[3]))))
shadowtext(x = 6, y = pred(6, slices), lab, pos = 1, col = "blue")

dev.off()
