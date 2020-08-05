getwd()
setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment2")


### 1 ###
install.packages("Hmisc")
library(Hmisc)

binconf(80, 124,alpha = 0.05)

binom.test(80,124,p=0.5,alternative='two.sided')

2*pnorm(-3.083)
