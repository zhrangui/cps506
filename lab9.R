install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)
prc <- read.csv("CMTH642 Lab 9 prostate_cancer_dataset.csv", stringsAsFactors = FALSE)
str(prc)
prc <- prc[-1]
table(prc$diagnosis_result)
prc$diagnosis <- factor(prc$diagnosis_result,
                        levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))
prop.table(table(prc$diagnosis))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
prc_n <- as.data.frame(lapply(prc[2:9], normalize))
prc_n <- cbind(prc$diagnosis, prc_n)

