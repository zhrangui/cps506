h <- rbinom(10000, 5, 0.5)
#hist(h)
hist(dbinom(h, 5, 0.5))
ppois(0, 720)
phyper(30, 100, 300, 125)
ppois() 
qpois(.5,12)
qbinom(1/32,5,.5)
qbinom(5/32,5,.5)

ppois(q=0, lambda = 12*60)
choose(5,3)
m = tagged = 100
n = diff = 300
k = sample = 125
x <- c(0:100)
plot(x, dhyper(x, m, n, k))

