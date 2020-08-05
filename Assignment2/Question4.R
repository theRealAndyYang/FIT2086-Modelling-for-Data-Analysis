getwd()
setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment2")

fuel <- read.csv("fuel2017-20.csv", header = TRUE)

summary(fuel)

fit<-lm(Comb.FE ~.,data=fuel)

summary(fit)

fit0.05/17

1.074e-01
-1.745e-01 

#Q4 Q5
fit.sw.bic = step(fit, k = log(length(fuel$Model.Year)))
fit.sw.bic%>%summary()


#Q6
fuel_test <- read.csv("fuel2017-20.test.csv", header = TRUE)

new_car <- predict(fit.sw.bic, newdata = fuel_test[1,], interval = "predict")

