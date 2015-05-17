#1 A pharmaceutical company is interested in testing a potential blood pressure lowering medication. 
#Their first examination considers only subjects that received the medication at baseline then two weeks later. 
#The data are as follows (SBP in mmHg)
Subject    Baseline	Week 2
1	140	132
2	138	135
3	150	151
4	148	146
5	135	130

week1 <- c(140,138,150,148,135)
week2 <- c(132,135,151,146,130)
diff <- week2 - week1
t.test(week1, week2, alternative = "two.sided", paired=T) # p value is 0.08

#2 A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is 
#the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null 
#hypothesis in a two sided 5% Students t-test?
n <- 9
mn <- 1100
s <-30
alpha <- 0.05
tstat <- qt(1-alpha/2, n-1) #2.3
mn +c(-1,1)*tstat*s/sqrt(n)

#3 Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of 
#two blinded drinks given in random order that they preferred. The data was such that 3 of the 4 people chose Coke.
#Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred 
#to Pepsi using a one sided exact test.
library(stats)
binom.test(x = 3, n = 4, p = .5, alt = "greater") # p-value = 0.3125

#4 Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high 
#and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 
#infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant 
#test of whether the hospital is *below* the standard?
p <- 1 / 100
pr <- 10 / 1787
n <- 1787
serror <- sqrt(p * (1-p) / n)
z <- (p-pr) / serror
pnorm(z, lower.tail = F) # 0.03066625

#5 Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body 
#mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo 
#for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for 
#the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences
#was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear to 
#differ between the treated and placebo groups? Assuming normality of the underlying data and a common population 
#variance, give a pvalue for a two sided t test.
n1 <- 9
n2 <- 9
df <- n1 + n2 - 2
meanTreat <- -3
meanPlacebo <- 1
sdTreat <- 1.5
sdPlacebo <- 1.8
pooledVar <- (sdTreat^2 * n1 + sdPlacebo^2 * n2)/df
se.diff <- sqrt(pooledVar/n1 + pooledVar/n2)
tstat <- (meanTreat - meanPlacebo) / se.diff
tstat
pValue <- 2 * pt(tstat, df = df)
pValue # 0.0001852248

#6 Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. Would you reject in a 
#two sided 5% hypothesis test of H0:μ=1,078?
# No calculation is needed. We wouldn't reject because the 95% CI contains
# the 90% CI. µ = 1078 falls within the interval, so we don't reject H0.

#7 Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume
#loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population 
#is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of 
#no volume loss?
n <- 100
mu <- .01
sd <- .04
power.t.test(n, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$power # 0.7989855

#8 Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of 
#.01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About 
#what would be the value of n needded for 90% power of type one error rate of 5% one sided test versus a 
#null hypothesis of no volume loss?
power <- .9
power.t.test(power = power, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$n # 138.3856







