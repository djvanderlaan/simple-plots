

pal <- hcl.colors("Viridis", n = nlevels(iris$Species))
col <- pal[iris$Species]

par(mar = c(3, 2, 1, 0) + 0.5)
plot(iris$Sepal.Width, iris$Petal.Width, pch = 20, type = 'n',
  ylab = "", bty = 'n', las = 1, xlab = "",
  xaxt = 'n', yaxt = 'n')
box(lwd =0.5, col = "black", bty = "o")
axis(1, lwd = 0, lwd.ticks = 1, cex.axis = 0.8, line = -0.7, tick = FALSE)
axis(2, lwd = 0, lwd.ticks = 1, las = 1, cex.axis = 0.8, line = -0.5, tick  = FALSE)
grid(lty = 1, col = "gray", lwd = 0.5)
points(iris$Sepal.Width, iris$Petal.Width, pch = 20, 
  col = col)
opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
  mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', 
  xlim = c(0,1), ylim = c(0,1), xaxs='i', yaxs='i')
text(0.01, 0.99, "Petal Width", font=2, adj = c(0, 1))
text(0.99, 0.01, "Sepal Width", font=2, adj = c(1, 0))
par(opar)
legend("right", legend = levels(iris$Species), fill = pal, border = NA,
  bty = 'n', horiz = FALSE, title = "Species", cex = 1, title.adj = 0)

