library(MASS)
t<-c(2019, 53.908, 106.079, "Waskesiu CFB", 40,
2019, 53.9714, 106.1311, "Birch Bay", 0.1,
2018, 53.908, 106.079, "Waskesiu CFB", NA,
2018, 53.595, 106.519, "Wasstrom's Flats", 834,
2018, 53.633, 106.541, "Millard", 1483,
2018, 53.6, 106.5, "Rabbit", 20228,
2018, 53.736, 106.097, "Sandy North", NA,
2018, 53.813, 106.018, "Namekus Lake", 1.2,
2017, 53.908, 106.079, "Waskesiu CFB", 56,
2017, 53.62, 106.52, "Millard", 693,
2017, 54.1022, 106.0631, "National", 0.5,
2017, 53.6, 106.6, "Wasstrom's Guards", 30,
2016, 53.586, 106.053, "South End Meadows", 830, ncol=5)
s<-sum(t, na.rm=T)
iMatrix=matrix(c(1, 2, 3,4, 2, 1, 2,3, 0), nrow=3)
t(iMatrix)
iv<-ginv(iMatrix)
i<-solve(iMatrix)
i<-iMatrix%*%iv
l<-c(1,2,NA)
mean(l,na.rm=T)