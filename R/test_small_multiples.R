


myplot <- function(x, y) {
  xrange <- range(x, na.rm = TRUE)
  yrange <- range(y, na.rm = TRUE)
  fun <- function(xlim, ylim) {
    if (missing(xlim)) xlim <- xrange
    if (missing(ylim)) ylim <- yrange
    plot(x, y, pch = 20, xlim = xlim, ylim = ylim)
  }
  structure(fun, xlim = xrange, ylim = yrange, class = "splot")
}

print.splot <- function(x, ...) {
  x()
}

myplot(iris$Sepal.Length, iris$Sepal.Width)




dta <- data.frame(
  x = iris$Sepal.Length, 
  y = iris$Sepal.Width,
  group = iris$Species
)
dta$group <- cut(iris$Petal.Length, 
  breaks = quantile(iris$Petal.Length, seq(0, 1,length.out = 15+1)),
  include.lowest = TRUE)

l <- split(dta, dta["group"])
l <- lapply(l, function(d) myplot(d$x, d$y))
xlim <- sapply(l, function(d) attr(d, "xlim"))
xlim <- range(xlim)
ylim <- sapply(l, function(d) attr(d, "ylim"))
ylim <- range(ylim)

l[[1]](xlim = c(0, 10), ylim = ylim)


nfigs <- length(l)
nrow  <- round(nfigs/3)
ncol  <- ceiling(nfigs/nrow)


par(mfrow = c(nrow, ncol)) 
lapply(l, function(l) {
  l(xlim = xlim, ylim = ylim)
})

