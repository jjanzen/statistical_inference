#1 Load the data set mtcars in the datasets R package. Calculate a 95% confidence interval to the nearest MPG for the variable mpg.
# What is the lower endpoint of the interval?
#What is the upper endpoint of the interval?

library(datasets)
data(mtcars)
attach(mtcars)
head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
t.test(mtcars$mpg) # 18 to 22
t.test(mtcars$mpg)$conf.int # 18 to 22 at 95%

# example from lecture to help with #1
# comparing study of those with sleep drug and without (connected groups)
data(sleep)
head(sleep,20)
extra group ID
0.7     1  1
-1.6     1  2
-0.2     1  3
library(ggplot2)
plot(sleep)
qplot(group, extra, data = sleep, main = "Tooth Length by Vitamin C Supplement")
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g
g1 <- sleep$extra[1:10]; g2 <- sleep$extra[11:20]
difference <- g2-g1
difference
#  [1] 1.2 2.4 1.3 1.3 0.0 1.0 1.8 0.8 4.6 1.4
mn <- mean(difference); s <- sd(difference); n <- 20
mn # 1.58  mean
c(-1,1) # -1,1  relative t quantive
qt(.975, n-1) # 2.09  evalue n -1 degrees of freedom
s/sqrt(n) # 0.28  standard error of interval
mn + c(-1,1) * qt(.975, n-1) * s/sqrt(n) # 1.00 2.156
t.test(difference)  # repeat on relativnt sample the mean at 95% confidence would end up 0.7 and 2.46 greater 
t.test(g1,g2,paird=T)

#2 Suppose that standard deviation of 9 paired differences is 1. 
# What value would the average difference have to be so that the lower endpoint of a 
# 95% students t confidence interval touches zero?
n <- 9
sd <- 1
avg diff + or i t.975 * sd/sqrt(n)
degreeFreedom <- n-1
qt(.975,degreeFreedom)*sd/3 # 0.77 is the avg diff

#4 Consider the mtcars dataset. Construct a 95% T interval for MPG comparing 
# 4 to 6 cylinder cars (subtracting in the order of 4 - 6) assume a constant variance.
head(mtcars)
# mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
g1 <- subset(mtcars$mpg,mtcars$cyl == 6)
g2 <- subset(mtcars$mpg,mtcars$cyl == 4)
t.test(g2,g1,var.equal=T) # 3.15 to 10.69 95% confident based on random sample of cars

t.test(mtcars$mpg) # 18 to 22

# chance of having at least 7 girls of 8 kids
n <- 8
pbinom(n-2, size = n, prob = .5, lower.tail = FALSE) # 3.5%

# calculating it manually
n/(2^n) # 3.1%

There are precisely 5 strings that have exactly 1 H and 4 T
So the required probability is 5/2^5

