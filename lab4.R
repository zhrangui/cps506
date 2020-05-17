lab4data <- read.csv("lab4dataset.csv", stringsAsFactors=FALSE, sep=",", na.strings=c("", "NA"))
str(lab4data)
mean <- function(dt) {
  sum(dt) / length(dt)
}

mean(lab4data$price)

z.score <- function (dt, x) {
  s = sd(dt)
  (x - mean(dt)) /s
}

z.score(lab4data$age, 89)

lower <- function(price, limit) {
  price[price <= limit] 
  
}

l  <- lower(lab4data$price, 100000)
length(l)/length(lab4data$price)

skew <- function(df) {
  if (median(df) < mean(df)) {
    return(1)
  } else if (median(df) == mean(df)) {
    return(0)
  } else {
    return(-1)
  }
}
average <- function(x)
{
  return(sum(x, na.rm = T)/length(na.omit(x)))
}
Skewness <- function(x)
{
  hist(x, freq = T, breaks = 50)
  return(sign(average(x) - median(x)))
}
Skewness(lab4data$price)
zscore <- function(x,y)
{
  return((x - average(y))/sd(y))
}
bath <- function(price)
{
  print(summary(lab4data$bathp[lab4data$price<=price]))
  return(zscore(average(lab4data$bathp[lab4data$price<=price]),
                lab4data$bathp))
}
#Example
bath(49120)