library("gtools")
x <- c(5, 10, 15)
perm <- permutations(length(x), 2, x, repeats.allowed=T)
rowMeans(perm)

sdm.sim <- function(n,src.dist=NULL,param1=NULL,param2=NULL) {
  r <- 10000 # Number of replications/samples - Not to be adjusted
  # This produces a matrix of observations with
  # n columns and r rows. Each row is one sample:
  my.samples <- switch(src.dist,
                       "E" = matrix(rexp(n*r,param1),r),
                       "N" = matrix(rnorm(n*r,param1,param2),r),
                       "U" = matrix(runif(n*r,param1,param2),r),
                       "P" = matrix(rpois(n*r,param1),r),
                       "C" = matrix(rcauchy(n*r,param1,param2),r),
                       "B" = matrix(rbinom(n*r,param1,param2),r),
                       "G" = matrix(rgamma(n*r,param1,param2),r),
                       "X" = matrix(rchisq(n*r,param1),r),
                       "T" = matrix(rt(n*r,param1),r))
  all.sample.sums <- apply(my.samples,1,sum)
  all.sample.means <- apply(my.samples,1,mean)
  all.sample.vars <- apply(my.samples,1,var)
  par(mfrow=c(2,2))
  hist(my.samples[1,],col="gray", freq = F,
       main="Distribution of One Sample")
  hist(all.sample.sums,col="gray", freq = F,
       main="Sampling Distribution of Sum")
  hist(all.sample.means,col="gray", freq = F,
       main="Sampling Distribution of Mean")
  hist(all.sample.vars,col="gray",
       main="Sampling Distribution of Variance")}
sdm.sim(200,"N", 0,1)

