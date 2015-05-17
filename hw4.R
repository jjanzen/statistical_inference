#1
#You want to test whether the true MPG is μ0 or smaller using a one sided 5% level test. (H0:μ=μ0 versus Ha:μ<μ0).
#Using that data set and a Z test: Based on the mean MPG of the sample xˉ, and by using a Z test: what is 
#the smallest value of μ0 that you would reject for (to two decimal places)?

library(datasets)
data(mtcars)
attach(mtcars)
head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4

mn <- mean(mtcars$mpg) # 20.09
s <- sd(mtcars$mpg) # 6.02
z <- qnorm(0.05) # -1.64
mu0 <- mn -z*s/sqrt(nrow(mtcars))
mu0 #21.8

#2 Consider again the mtcars dataset. Use a two group t-test to test the hypothesis that the 4 and 6 cyl cars have the
#same mpg. Use a two sided test with unequal variances.
#Do you reject at the 5% level (enter 0 for no, 1 for yes).
#What is the P-value to 4 decimal places expressed as a proportion?
g1 <- subset(mtcars$mpg,mtcars$cyl == 6)
g2 <- subset(mtcars$mpg,mtcars$cyl == 4)
t.test(g2,g1,var.equal=F)

#3 A sample of 100 men yielded an average PSA level of 3.0 with a sd of 1.1. What are the complete set of 
#values that a 5% two sided Z test of H0:μ=μ0 would fail to reject the null hypothesis for?
#Enter the lower value to 2 decimal places.
#Enter the upper value to 2 decimal places.
n <- 100
mn <- 3
s <- 1.1
z <- qnorm(0.05)
alpha <- 0.05
mn + c(-1,1) * qnorm(1-alpha/2) * s/sqrt(n) # 2.78 3.22

#4 You believe the coin that you're flipping is biased towards heads. You get 55 heads out of 100 flips.
#What's the exact relevant pvalue to 4 decimal places (expressed as a proportion)?
#Would you reject a 1 sided hypothesis at α=.05? (0 for no 1 for yes)?
n <- 100
alpha <- 0.05
Ho: p = .5, Ha: p > .5
pbinom(54, size=100, prob = .5, lower.tail=F) # 0.18
# or could use pnrom 
pnorm(0.55, mean = 0.5, sd = sqrt(0.5*0.5/100), lower.tail=F) # 0.16

#5: A web site was monitored for a year and it received 520 hits per day. In the first 30 days in the next year, 
#the site received 15,800 hits. Assuming that web hits are Poisson.
#Give an exact one sided P-value to the hypothesis that web hits are up this year over last to four 
#significant digits (expressed as a proportion).
#Does the one sided test reject (0 for no 1 for yes)?
Ho: lambda = 520, Ha: lambda > 520
mn <- 520
n <- 30
ppois(15800-1, lambda=mn*30, lower.tail=F ) # 0.0553
pnorm(15800, mean= mn*n, sd = sqrt(520*30), lower.tail=F) # 0.55

#6 Suppose that in an AB test, one advertising scheme led to an average of 10 purchases per day for a sample 
#of 100 days, while the other led to 11 purchaces per day, also for a sample of 100 days. Assuming a 
#common standard deviation of 4 purchases per day. Assuming that the groups are independent and that they days 
#are iid, perform a Z test of equivalence.
#What is the P-value reported to 3 digits expressed as a proportion?
#Do you reject the test? (0 for no 1 for yes).
h0: old => new ha: old < new
mn <- 10
n <- 100
mn_test <- 11
s <- 4
se <- s * sqrt(1/n + 1/n) # this is the variance
z <- di
difference = mn - mn_test
z <- difference/se
z # 1.77
2 * pnorm(z) # 0.077  since this is > than 0.05 for 95% CI, it's not enough to reject ho

#8 Consider two problems previous. Assuming that 10 purchases per day is a benchmark null value, that days 
#are iid and that the standard deviation is 4 purchases for day. Suppose that you plan on sampling 100 days. 
#What would be the power for a one sided 5% Z mean test that purchases per day have increased under the 
#alternative of μ=11 purchase per day?
#Give your result as a proportion to 3 decimal places.
h0: mu = 10 ha: mu > 10
Ho is normally distributed with mean 10 and sd is 4/sqrt(100), going to reject is that lyes in upper 5% of tail
#1.65 is upper 5%
mn <- 10
s <- 4
n <- 100
mn_alt <- 11
se <- s/sqrt(n)  # stardard error 0.4
adjusted_norm <- mn + 1.645 * se
pnorm(adjusted_norm, mean=11, sd=0.4, lower.tail=F) # 80.4  - which means 80% power

# 9 Researchers would like to conduct a study of healthy adults to detect a four year mean brain volume loss of 
#.01 mm3.Assume that the standard deviation of four year volume loss in this population is .04 mm3.
#What is necessary sample size for the study for a 5% one sided test versus a null hypothesis of no 
#volume loss to achieve 80% power? (Always round up)
Ho: mu = 0, Ha: mu > 0
sd <- 0.04
power <- 80%
mua <- 0.01
pnorm(0, 0.04/sqrt(n))
m > 0 + 1.645 * sd/sqrt(n)
pnorm(0.0658/sqrt(n), mean=0.01, sd = sd/sqrt(n), lower.tail=F)
n <- 99
