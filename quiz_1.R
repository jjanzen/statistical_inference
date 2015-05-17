#1
p(A) = .12
p(B) = solve
p(A or B) = .17  (aka union)
p(A & B) = .6  (aka interect on ven diagram)

p(A or B) = p(A) + p(B) - p(A & B)
.17 = .12 +p(B) - .6
p(B) = .11

#2 A random variable, X is uniform, a box from 0 to 1 of height 1. (So that its density is f(x)=1 for 0≤x≤1.) What is its 75th percentile? 
.75

#3
heads = x
tails = y

p(x) = sigma
p(Y) = 1 - sigma

#5
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp
mean(temp)
sum(p)
(x *p)/4
1 * .1 + 2 * .2 +  3 * .3 + 4 * .4 

#6 
sensitivity = 75%
specificity = 52%
subject has positive test and that 30% taking the test are pregnant
What number is closest to the probability of pregnancy given the positive test?

D = prenact
Dc = not preganet
sensitivity = P(+ | D)  pos test and actually preg
specifivty = P(- | Dc)  neg test and actually not preg
prevalence = anyone test the test is pregnat

Dignistic likelihood ration = .75 / (1-.52)
DLR = .75 / (1-.52) = 1.5625

P(+ | D) = .75
P(+ but DC) = 1 - .75 = .25

P(- | DC) = .52
P(- but D) = 1 - .52 = .48

P(D) = .3

.3*.75
.3*.48
# uncondition prob
P(+) 
.75*.3 + .25*.48 = .345

P(D | +)
(.75 * .3)/.345

    









