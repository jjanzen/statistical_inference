# part 1

# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter
# mean of exponential distribution is 1/lambda 
# and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations. 
# You will investigate the distribution of averages of 40 exponentials. 
# Note that you will need to do a thousand simulations.

# give rate parameter 
lamdba <- 0.2
# number of exponentials
n <- 40
# number of simulations to invensigate distribution
nosim <- 1000
set.seed(10)

# create simulations (samples) using 1000 simulations (rows) and values of each (columns)
## matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
simulation <- matrix(rexp(nosim*n, rate=lambda), nosim, n)
head(simulation)

# get mean of each of the 1,000 simulations
row_means <- rowMeans(simulation)

# create historgram of means of 1,000 simuations
hist(row_means, breaks = 25, prob=T, main = "Distribution of Means of 1k Simulations", xlab="Frequency")
# add density line of best fit for mean of simulations
lines(density(row_means))

# center of theoretical distribution
abline(v=1/lambda, col="red")
# theoretical density of mean of simulations
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(n)))
lines(xfit, yfit, pch=22, col="red", lty=2)
# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), col=c("black", "red"))

# simulation mean
mean(row_means)
# 4.978

# theoritical mean
lambda^-1
# 5.0  

# simulation variance
var(row_means)
# 0.628

# theoritcal variance
1/(lambda^2*n)
# 0.625

# plot of variance of simulation to variance of theoritical distribution
qqnorm(row_means,main = "Var of Simulations vs. Var of Theoritical Distributions"); qqline(row_means)

?qqnorm
# confidence interval 
?seq # seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL, along.with = NULL, ...)
# generate values from 4 to 6 by .01
lambda_vals <- seq(4, 6, by=0.01)
lambda_vals
coverage <- sapply(lambda_vals, function(lamb) {
    mu_hats <- rowMeans(simulation)
    ll <- mu_hats - qnorm(0.975) * sqrt(1/lambda**2/n)
    ul <- mu_hats + qnorm(0.975) * sqrt(1/lambda**2/n)
    mean(ll < lamb & ul > lamb)
})

library(ggplot2)
?qplot
qplot(lambda_vals, coverage, main = "Confidence Interval of Mean Simulations") + geom_hline(yintercept=0.95)

# part 2
# Load the ToothGrowth data and perform some basic exploratory data analyses 
# Provide a basic summary of the data.
# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
# (Only use the techniques from class, even if there's other approaches worth considering)
# State your conclusions and the assumptions needed for your conclusions. 
library(datasets)
data(ToothGrowth)
attach(ToothGrowth)

head(ToothGrowth,60)
#len supp dose
#1  4.2   VC  0.5
summary(ToothGrowth)
str(ToothGrowth)
library(ggplot2)
qplot(dose, len, data = ToothGrowth, facets=~supp, main = "Tooth Length by Vitamin C Supplement")

order_tooth_growth <- ToothGrowth[with(ToothGrowth,order(dose)),]
g1 <- ToothGrowth$len[1:20]; g2 <-ToothGrowth$len[21:40]; g3 <- ToothGrowth$len[41:60];

head(order_tooth_growth, 25)

difference_2_1 = g2-g1
#t.test(difference_2_1)
t.test(g2, g1, paired = F)
#t.test(len ~ I(relevel(supp, 2)), paired = TRUE, data = order_tooth_growth)

difference_3_2 = g3-g2
#t.test(difference_3_2)
t.test(g3, g2, paired = F)
#t.test(len~ I(relevel(group, 2)), paired = TRUE, data = order_tooth_growth)

difference_3_2 = g3-g2
#t.test(difference_3_2)
t.test(g3, g1, paired = F)
#t.test(len~ I(relevel(group, 2)), paired = TRUE, data = order_tooth_growth)


?qplot
data(sleep)
head(sleep,20)
str(sleep)

g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1

# t conf interverval 
mn <- mean(difference); s <- sd(difference); n <- 10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)
# 95% of sample means will fall within these values

rbind(
    mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
    as.vector(t.test(difference)$conf.int),
    as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
    as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)