


base_circle <- function(n = 25) {
  phi <- seq(0, 2*pi, length.out = n+1) + 0.5*pi
  phi <- head(phi, -1)
  x <- cbind(cos(phi), sin(phi))
  colnames(x) <- c("x", "y")
  x
}


n <- 20
x <- runif(n, 0, 100)
y <- runif(n, -1, 1)
r <- runif(n, 0.1, 1)*5

# xr <- diff(range(x))
# yr <- diff(range(y))
# asp <- yr/xr


xmax <- max(x+r)
xmin <- min(x-r)
ymax <- max(y+r)
ymin <- min(y-r)
plot(x, y, xlim = c(xmin, xmax), ylim = c(ymin, ymax), pch = 20)


w <- par("pin")[1]/diff(par("usr")[1:2])
h <- par("pin")[2]/diff(par("usr")[3:4])
asp <- w/h


px <- mapply(function(x, y, r, asp) {
    c <- base_circle(n=6) * r
    c[,1] <- c[,1]+x
    c[,2] <- c[,2]*asp+y
    c <- rbind(c, c(NA, NA))
    c[,1]
  }, x, y, r, asp = asp)
py <- mapply(function(x, y, r, asp) {
    c <- base_circle(n=6) * r
    c[,1] <- c[,1]+x
    c[,2] <- c[,2]*asp+y
    c <- rbind(c, c(NA, NA))
    c[,2]
  }, x, y, r, asp = asp)

polygon(px, py, pch = '.', lty =3)



