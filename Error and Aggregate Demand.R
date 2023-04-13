
# use the c() function to create a vector of data
x <- c(15.6, 16.2, 22.5, 20.5, 16.4, 19.4, 16.6, 17.9, 12.7, 13.9)

# calculate sample mean of x
sum(x)/length(x)

xbar <- sum(x)/length(x)

#alternatively, use the mean()

#Calculate Standard deviation of x
s <- sd(x)

#calculate t statistic
# null hypothesis: mu=20
mu = 20
n = length(x)

t <- (xbar - mu)/(s/sqrt(n))

#find t crit
# use the qt() function
tcrit = qt(0.01, n-1)

#alternatively, find the p-value
p = pt(t, n-1)



x2 = c(6, 7, 5.2, 9.3, -9.6, 0.9, 1, 8.1, 9, 5.4)
xbar2 = mean(x2)
x3 = c(-1.8, 5.1, 9.1, -1.3, 0, 9.1, 1.3, -3.9, -9.3, -4)
s3 = sd(x3)
x4 = c(-3.1, 2.3, -6.2, 0, -3, -5.2, -1.4, 8.8, -10, -8.6)
lnx4 = log(x4, base = exp(1))

x6 = c(4.7, -4.4, 5.9, -0.7, -3.3, -4.1, 3.2, -6.1, 5.1, 8.7)
n6 = length(x6)
s6 = sd(x6)
z6 = 1.96
xbar6 = mean(x6)
a6 = xbar6 - (z6)*(s6/n6)
error6 = qt(.95,df=n6-1)*s6/sqrt(n6)
left6 = xbar6 - error6

#Problem 7
x7 = c(7.3,-2.2,8.4,7.5,-2.9,7,-6,0.6,-2.1,1.9)
n7 = length(x7)
s7 = sd(x7)
xbar7 = mean(x7)
error7 = qt(.975,df=n7-1)*s7/sqrt(n7)
right7 = xbar7 + error7

#Problem 8
x8 = c(6.8,3.2,7.4,-2.5,-4.8,-8.9,-6.1,0.1)
xbar8 = mean(x8)
s8 = sd(x8)
n8 = length(x8)
t8 = ((xbar8 + 3.1)/(s8/sqrt(n8)))

#Problem 9
x9 = c(7.2,9.2,0.4,-2.8,-4,1.2,-8.8,9.5,1.3)
xbar9 = mean(x9)
s9 = sd(x9)
n9 = length(x9)
t9 = ((xbar9 + 1)/(s9/sqrt(n9)))
tcrit9 = qt(.05, n9-1)
