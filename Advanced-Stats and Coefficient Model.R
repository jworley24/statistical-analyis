library(ggplot2)

## Parameters
b0 <- -4
b1 <- 3

## Make "population" data
df <- data.frame(x = rnorm(10000),
                 u = rnorm(10000))
df$y <- b0 + b1*df$x + df$u

## Function to create sample of size n from population
sample_fun <- function(n){
  ind <- sample(1:length(df$y), n)
  df_samp <- df[ind,]
  return(df_samp)
}

## Function to perform OLS on sampled data,
##  extract estimate for beta 1
ols_fun <- function(dat){
  model <- lm(y ~ x, data=dat)
  b1hat <- coefficients(model)[2]
  return(b1hat)
}

## Function to return 1000 estimates at sample size n
beta1hat_fun <- function(n){
  beta1hat_vec <- sapply(1:1000, function(x){
    dat <- sample_fun(n)
    beta1hat <- ols_fun(dat)
    return(beta1hat)
  })
  return(beta1hat_vec)
}

slr = lm(y ~ x, data=df)
summary(slr)

q1 = var(beta1hat_fun(10))
q2 = var(beta1hat_fun(100))
qplot()

n2 = var(beta1hat_fun(200))
n3 = var(beta1hat_fun(300))

x3 = c(3,6,3,9,7)
y3 = c(4,2,5,2,1)
qplot(y3, x3)
n100 = var(sample_fun(100))

var(sample_fun(100))

var(ols_fun(100))

n100 = var(beta1hat_fun(100))
n200 = var(beta1hat_fun(200))
n300 = var(beta1hat_fun(300))
n400 = var(beta1hat_fun(400))
n500 = var(beta1hat_fun(500))
n600 = var(beta1hat_fun(600))

VarOls = c(beta1hat_fun(100), beta1hat_fun(200), beta1hat_fun(300),
           beta1hat_fun(400), beta1hat_fun(500), beta1hat_fun(600),
           beta1hat_fun(700), beta1hat_fun(800), beta1hat_fun(900),
           beta1hat_fun(1000))
Samplesize = c(100,200,300,400,500,600,700,800,900,1000)
VarOls = sapply(VarOls, var)
qplot(Samplesize, VarOls)