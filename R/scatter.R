
scatter <- function(x, y, r, colour = "black", border = colour, alpha = 0.2, 
    add = FALSE, pars_grid = parameters_grid()) {
  # When r is missing we just draw points
  draw_bubble <- !(missing(r) || is.null(r))
  if (!draw_bubble) r = 0
  # This bit somewhat complicated: the radius should be given in the units that
  # are on the x-axis. However, the units on the y-axis can be completely 
  # different. To determin the x-range we can just do min(x-r) to max(x+r). For,
  # y we need to scale the units to get a proper circle. The scaling factor is 
  # calles asp below. Another complication is that our figure is probably not 
  # square. The quareness of our figure is measured using asps (this is an 
  # approximation). Below we try to estimate a good value for asp. The true 
  # value of asp can only be calculated when the figure is drawn however, we 
  # need the limits first. 
  s <- dev.size()
  # Perhaps we could use par("pin") but a figure first needs to be open
  asps <- s[1]/s[2]
  asp <- 1
  for (i in 1:3) {
    xlim <- c(min(x-r), max(x+r))
    ylim <- c(min(y-asp*asps*r), max(y+asp*asps*r))
    asp <- diff(ylim)/diff(xlim)
  }
  # Open plot
  if (!add) {
    plot(x, y, xlim = xlim, ylim = ylim, type = 'n', 
      bty = 'n', xaxt = 'n', yaxt = 'n', xlab ="", ylab = "")
    # Todo allow user to specify properties of axes/grid etc.
    axis(1, lwd = 0, lwd.ticks = 0)
    axis(2, lwd = 0, lwd.ticks = 0, las = 1)
    if (pars_grid$show) do.call(grid, pars_grid)
  }
  # Draw
  if (draw_bubble) {
    fill <- adjustcolor(colour, alpha.f = alpha)
    symbols(x, y, circles = r, inches = FALSE, fg = border, 
      bg = fill, add = TRUE)
  } else {
    points(x, y, pch = 21, col = border, bg = colour)
  } 
}

parameters_grid <- function(show = TRUE, col = "black", lty = 3, 
    lwd = 1, ...) {
  c(list(show = show, col = col, lty = lty, lwd = lwd), list(...))
}





scale_color <- function(x, n = 9, 
    palette = list(factor = "Dark2", numeric = "viridis")) {
  if (is.character(palette)) 
    palette <- list(factor = palette, numeric = palette)
  if (is.numeric(x)) {
    breaks <- pretty(x, n = n)
    c <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
    pal <- hcl.colors(n = n, palette = palette$numeric)
    return(pal[as.integer(c)])
  } 
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) {
    n <- nlevels(x)
    pal <- hcl.colors(n = n, palette = palette$factor)
    return(pal[as.integer(x)])
  }
}


scale_color <- function(x, n = 9, 
      palette = list(factor = "Dark2", numeric = "viridis")) {
  if (is.character(palette)) 
    palette <- list(factor = palette, numeric = palette)
  if (is.numeric(x)) {
    breaks <- pretty(x, n = n)
    pal <- hcl.colors(n = n, palette = palette$numeric)
    c <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
    res <- function(x) {
      c <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
      pal[as.integer(c)]
    }
    res <- structure(res, breaks = breaks, levels = levels(c), pal = pal)
  } 
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) {
    n <- nlevels(x)
    pal <- hcl.colors(n = n, palette = palette$factor)
    res <- function(x) {
      pal[as.integer(x)]
    }
    res <- structure(res, levels = levels(x), pal = pal)
  }
  res
}


x<- iris$Sepal.Length


scale <- scale_color(iris$Species, palette = "Dynamic")

scatter(iris$Sepal.Length, iris$Sepal.Width, colour = scale(iris$Species),
  border = "black")

hcl.colors(n = 30, palette = "Dark 2")



if (FALSE) {
  pal <- hcl.colors(n = 10)
  n <- 50
  x <- runif(n, 0, 100)
  y <- x * 0.1 *runif(n, -1, 1)
  r <- 1+0.01*x*runif(n, 0.1, 1)*5
  col <- pal[sample(1:10, n, replace = TRUE)]
  
  par(mar = c(4, 3, 2, 0)+0.2)
  scatter(x, y, r, col)
  mtext(side = 3, "Blascore",  line = -2, adj = 0.02, outer = TRUE, 
    cex = 1.2)
  mtext(side = 1, "Aantal foobars", line = 2.5, adj=0.98, cex = 1.2)

}
