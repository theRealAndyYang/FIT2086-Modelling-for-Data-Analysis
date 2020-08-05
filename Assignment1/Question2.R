setwd("C:/Users/yjb13/OneDrive/Monash Uni/Year 2/FIT2086 Modelling for Data Analysis/Assignment1")
PA <- read.csv("port.adelaide.csv", header = FALSE)$V1
PA
previous0_0 <- 0
previous0_1 <- 0
previous1_0 <- 0
previous1_1 <- 0
for (i in 2:length(PA)) {
    if (PA[i-1] == 0) {
      if (PA[i] == 0) {
        previous0_0 <- previous0_0 + 1 
      } else {
        previous0_1 <- previous0_1 + 1 
      }
    }
    if (PA[i-1] == 1) {
      if (PA[i] == 0) {
        previous1_0 <- previous1_0 + 1 
      } else {
        previous1_1 <- previous1_1 + 1
      }
    }
}

previous0_0
previous0_1
previous1_0
previous1_1

total = length(PA)-1
total

total_win <- 0
for (i in 3:length(PA)) {
  if (PA[i] == 1) {
    total_win = total_win + 1
  }
}
total_win
prob_total_win = total_win/total



previous0_0/total/prob_total_win
previous0_1/total/prob_total_win
previous1_0/total/prob_total_win
previous1_1/total/prob_total_win






