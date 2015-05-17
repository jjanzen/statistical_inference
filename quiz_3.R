In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. 
What is a 95% Student's T confidence interval for the mean brain volume in this new population?
# http://www.cyclismo.org/tutorial/R/confidence.html
# normal diest
mean <- 1100
n <- 9
sd <- 30
alpha <- 0.05
error <- qnorm(1-alpha/2)*sd/sqrt(n)
left <- mean-error
left #1080
right <- mean+error
right #1120

# t dist
mean <- 1100
n <- 9
sd <- 30
alpha <- 0.05
error <- qt(1-alpha/2,df=n-1)*sd/sqrt(n)
left <- mean-error
left #1099
right <- mean+error
right #1101
mean + c(-error,error)
# 1076.94 1123.06

# 2
#A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) 
#is -2 pounds. What would the standard deviation of the difference in weight have to be for the upper 
#endpoint of the 95% T confidence interval to touch 0?

mean <- 2
n <- 9
alpha <- 0.05 # for 95% CI
tvalue <- qt(1-alpha/2,n-1) # 2.3
sd <- mean*sqrt(n)/ tvalue
sd # 2.6

# 3
In an effort to improve running performance, 5 runners were either given a protein supplement or placebo. 
#Then, after a suitable washout period, they were given the opposite treatment. Their mile times were 
#recorded under both the treatment and placebo, yielding 10 measurements with 2 per subject. 
#The researchers intend to use a T test and interval to investigate the treatment. 
#Should they use a paired or independent group T test and interval?
# answer - paird

# 4
#In a study of emergency room waiting times, investigators consider a new and the standard triage systems. 
#To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be 
#used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median 
#waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance 
#of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% 
#confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a 
#constant variance. What is the interval? Subtract in this order (New System - Old System).

n <- 10
mean_new <- 3
var_new <- 0.6
sd_new <- (var_new)
mean_old <- 5
var_old <- 0.68
sd_old <- (var_old)
alpha <- 0.05 

error_new <- qt(1-alpha/2,df=n-1)*sd_new/sqrt(n)
mean_new + c(-error_new,error_new)

error_old <- qt(1-alpha/2,df=n-1)*sd_old/sqrt(n)
mean_old + c(-error_old,error_old)
2.57-4.51
3.43-5.48

n_x <- 10 
n_y <- 10
x_bar <- 5 # old_system
y_bar <- 3 # new_system
var_x <- 0.6
var_y <- 0.68
alpha <- 0.05
sp_2 <- ((n_x - 1)*var_x + (n_y - 1)*var_y) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((y_bar - x_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2) 

# Problem 5.
# 90% confidence interval gives a lower t-value then 95% confidence interval. 
# => The interval will be narrower.

# Problem 6.
n_x <- 100
n_y <- 100
x_bar <- 6
y_bar <- 4
s_x <- 2
s_y <- 0.5
alpha <- 0.05
sp_2 <- ((n_x - 1)*s_x^2 + (n_y - 1)*s_y^2) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((x_bar - y_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2) 
# 1.59 2.41 => The new system appears to be effective. 

n1 <- n2 <- 100
xbar1 <- 4
xbar2 <- 6
s1 <- 0.5
s2 <- 2
xbar2 - xbar1 + c(-1, 1) * qnorm(0.975) * sqrt(s1^2/n1 + s2^2/n2)

# 7
n_treated <- 9
mean_treated_diff <- -3
n_untreated <-
mean_untreated_diff <- 1
sd_treated <- 1.5
sd_untreated <- 1.8
alpha <- 0.1
difference <- mean_treated_diff - mean_untreated_diff
mn <- mean(difference);
mn + c(-1,1) * qt(1-alpha/2, n_treated-1) * sd_treated/sqrt(n_treated)

n_x <- 9
n_y <- 9
x_bar <- -3
y_bar <- 1
s_x <- 1.5
s_y <- 1.8
alpha <- 0.1
sp_2 <- ((n_x - 1)*s_x^2 + (n_y - 1)*s_y^2) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((x_bar - y_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 3) 
# -5.364 -2.636

n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
(x1 - x2) + c(-1, 1) * qt(0.95, n1 + n2 - 2) * s * sqrt(1/n1 + 1/n2)