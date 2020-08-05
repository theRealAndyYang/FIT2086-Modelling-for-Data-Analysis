### 1 ###

y <- seq(0, 10, length.out = 1000)

v = 2

prob = exp(-exp(-v) * y - v)

dat <- data.frame(y = y, prob =prob)

library(ggplot2)

ggplot(dat, aes(x = y, y = prob)) + geom_line() + xlab("y") + ylab("Probability") + ggtitle("Question 2.1")



