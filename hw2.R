#2 web traffic
Website web hits are approx normally distributed, and what is the probability that get fewer than 50 hits/day

# web traffic for last week
web_visits <- c(64,34,55,47,52,59,77)
visits_day <- mean(web_visits) # mean = 55.4
sd_visits_day <- sd(web_visits) # standard_deviation = 13.5
goal_visits <- 50

# for reference only
stan_deviations_below_mean <-  (goal_visits - visits_day)/sd_visits_day # -0.4

#result
pnorm(visits_day,goal_visits,sd=sd_visits_day,lower.tail=F)
# .344 or 34.4%


m = 100 hits/day
std = 10 hits/day

what is prob that a day has < 93 hits

(93-100)/10 = -0.7

pnorm(93,100,sd=10,lower.tail=T)
# 0.24
or 
pnorm(-0.7)
# 0.24

#3 (Baynes)
5% of housing projects has asbstos
sensitivity test = 93%
specificity = 88%
what is prob no asbestos given a negative test
The sensitivity is the probability that the test is positive given that the subject actually has the disease, $P(+ ~|~ D)$
The specificity is the probability that the test is negative given that the subject does not have the disease, $P(- ~|~ D^c)$

prob_pos <- (.93*.05)/(.93*.05 + (1-.88)*(1-.05))
prob_pos
prob_none_neg <- (.88*(1-.05))/(.88*(1-.05) + (1-.93)*.05)
prob_none_neg
#0.995

dlr_ab <- .93*(1-.88)
dlr_ab

#4 web traffic - find % of days over threshold
m = 100 hits/day
std = 10 hits/day

what is num hits where 5% of day have more hits

qnorm(0.95)
# 1.65
100+1.65*10 

or
round(qnorm(.05,mean=100,sd=10,lower.tail=F),3)
# 116.449

#5 web traffic - number of web hits where 5 percentile
m = 100 hits/day
std = 10 hits/day

50 random samples, so need to calc new std
hw5_std <- 10/sqrt(50)

qnorm(.95, mean=100, sd = 10/sqrt(50))
#102.3262

#5 Binomonal P-value question
6 pairs wines in taste test, and got 5 of 6 right
pbinom(4, size = 6, prob = 0.5, lower.tail = F)

#6 Central Limit Theroum - uniform dist
sample = 100
mean = 0.5
var = 1/12
sigma = sqrt(1/12)/sqrt(100)
sigma
# 0.03
what is prob getting .51 or larger
round(pnorm(.51,mean=mean,sd=sigma,lower.tail=F),3)

#7 rolled a dice 10 times and took avg and historgram, what would be middle
1:6
#3.5

#8 same as above but what wouldl be variance
the var and die rolls is 2.92
2.92/10
mean((1:6 - 3.5)^2/10)

#9 web hit is Poisson with mean 16.5 per day
mean = 16.5
what is prob getting 20 or fewer in 2 day as a %
mean = variance in Poisson, denoted by lambda
ppois(20, lambda=16.5*2)
# 1%

example:  2.5 per hour, if 4 hours, what is prob 3 or few show up whole time
ppois(3, lambda=2.5*4)
