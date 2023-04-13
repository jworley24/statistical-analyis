library(wooldridge)
library(car)
data("lawsch85")

summary(lawsch85)

model1 = lm(salary ~ LSAT + GPA + rank, data=lawsch85)
summary(model1)

## test H0: beta3 = -1%
## calculate the t
t = (coefficients(model1)["rank"]+0.01)/0.00029999

# do this without copy/paste
t_new = (coefficients(model1)["rank"] + 0.01)/summary(model1)$coefficients[4,2]

## Rule of thumb: reject H0 if |t| > 2
## so we reject

## even better way (using car package)
library(car)
linearHypothesis(model1, "rank = .01")
linearHypothesis(model1, "GPA = 0")
