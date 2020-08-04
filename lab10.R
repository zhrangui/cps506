install.packages('FNN')
install.packages('RCurl')
install.packages('MASS')
install.packages('leaps')
library(MASS) # stepwise regression
library(leaps) # all subsets regression
library(RCurl) # getURL
u <- getURL("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Computers.csv")
c_prices <- read.csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Computers.csv")
