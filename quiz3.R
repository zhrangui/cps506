y <- matrix(nrow=2, ncol=3, 1:6)
y[,2]
y[ncol=2]

bugsTotal <- c(40, 35, 60, 45)
bugsSeverity <- c("Low", "Medium", "High", "Critical" )
TrackingFrame <- data.frame(bugsSeverity, bugsTotal)
TrackingFrame[2, ]

bugs <- c(1:5)
patches <- c(2, 17, 8, 11, 5)
Issues <- data.frame(Bugs=bugs, Patches =patches)

Issues$Patches[ 1 : 5]
Issues[2,]
Issues[,2]
x <- c(1, 2, NA, 4, NULL, 5)
is.na(x)
l <- list(3, "T")
x <- c(1, 2, 3); y <- c(1, 2, 3, 4)
z = x + y
Y <- list(c(1,2,3), "Toronto")
z <- c(1,NA,2,3)
mean(z)
.776*9
charCounter <- function(Str)
{
  nchar(Str)
}
charCounter("Hello, world!\n")


f <- function(a)
{
  c(1:a)
}
f(4) - f(2)

f <- function(a, b)
{
  x <- c(a:b)
  y <- c(b:a)
  x[a] - y[b]
}
f(1,4)

f <- function(a)
{
  7
  8
}
f(9)
