bottles<-c(484.11, 459.49, 471.38, 512.01, 494.48, 528.63, 493.64, 485.03, 473.88,
501.59, 502.85, 538.08, 465.68, 495.03, 475.32, 529.41, 518.13, 464.32, 449.08, 489.27)
mean(bottles)
sd(bottles)
t.test(bottles, mu=500, alternative="less", conf.level=0.99)
x = c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)
t.test(x, mu=0.3)
t.test(x, alternative="less", mu=0.3)
t.test(x, alternative="greater", mu=0.3)
mtcars$am == 0
