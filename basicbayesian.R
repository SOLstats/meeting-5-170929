# Binomial outcome: yes or no. Say, you are interested in a proposal of building a 
# new shopping mall in the center of your home town. Respondents can either say "yes" or "no".

# Is there a bias towards yes or no, or are the opinions equally divided? You ask 100 people. 
# Traditional approach could be something like this:

# H0: p(yes) = p(no) = 0.50. Write this down as a conditional probability on the whiteboard.
# H1: p(yes) != 0.50

# Experiment 100 trials, of which 60 are positive (yes).
experiment=c(rep(1,60),rep(0,40))

# How likely is an outcome of 60 or more under the H0?

# Plot a binomial distribution based on 100 trials.
plot(seq(0,100),dbinom(x=0:100,size=100,prob=0.5),type="h",ylab="Probability",xlab="Number of positive outcomes",
	 col=c(rep("black",60),rep("red",41)))

# The vertical lines are probabilities of the individual outcomes. They sum up to 1:
sum(dbinom(x=0:100,size=100,prob=0.5))

# The red lines indicate the probabilities of the outcome of the experiment, or more extreme: x >= 60.

# What is the sum of the red lines in the graph?
sum(dbinom(x=61:101,size=100,prob=0.5)) # probability is 0.017 which is significant with p < .05.

# The same result can be obtained with pnorm():
pbinom(q=60,size=100,prob=0.5,lower.tail=F)

# Confidence interval:
plogis(confint(glm(experiment~1,family="binomial")))

# Normal distribution approach to this (this is not quite a correct approach). 
mean(experiment) # The mean value of the sample is 0.60
sd(experiment)
t.test(experiment,mu=0.5) # Signifcant results, also shows the confidence interval.

# Bayesian approach

# In the Bayesian approach, there is a parameter (labeled theta), about which you want
# find out information. In the present example, theta is the force that generated the data.
# Practically speaking, it is the expected value of the proportion yes responses

# In the Bayesian approach, it is assumed that there is a range of possibilities for what the actual 
# value of the probability parameter is. It may vary from 0 to 1, with some values more likely than other values.

# 1. There is a likelihood function, that is, if you try different values of theta (everything 
# between zero and one), how likely is it that you get the observed outcome?

# In the case of the present example, the likelihood function is
theta ^ nr YES times (1 - theta) ^ nr NO  # write on the board.

# Example: if theta = 0.5 then:
(0.5^60 * 0.5^40)

# If theta is 0.7 then
(0.7^60 * 0.3^40)

# If theta is 0.6 then
(0.6^60 * 0.4^40)

# For many values between 0 and 1:
theta=seq(0,1,0.001) # 1001 values
plot(theta,theta^60*(1-theta)^40, type="l",main="likelihood function",ylab="likelihood")
abline(v=0.6) # Highest point at 0.60.

# 2. There is a prior belief about the parameter. 
# You specify a prior belief about the parameter, using a probability density function
plot(seq(0,1,length.out=100),dbeta(seq(0,1,length.out=100),1,1),type="l") # Surface under the 'curve' equals 1.
plot(seq(0,1,length.out=100),dbeta(seq(0,1,length.out=100),4,4),type="l") # Surface under the 'curve' equals 1.

# The beta distribution is so-called conjugate on the binomial distribution.
# That means that a beta-distribution with parameters a and b is 'close to' 
# a binomial distribution with parameters (a-1) and (b-1).

par(mfrow=c(1,2))
theta=seq(0,1,0.001) # 1001 values
plot(theta,dbeta(theta,7,7),type="l",main="Beta distribution, a = 7, b = 7")
plot(theta,theta^6*(1-theta)^6,type="l",main="Binomial distribution, N = 12, z = 6")

# There is an additional correction that needs to be made so that the areas
# under the two curves become equal.

# 3. Posterior distribution

# The likelihood is then multiplied with the prior to get to a posterior
# function/distribution. 
# The curves are plotted together in a single graph for comparison.

# 3.1 Prior is beta(2,2)
par(mfrow=c(1,3))
plot(theta,dbeta(theta,2,2),type="l",main="Weak prior",ylim=c(0,10))
lines(theta,dbeta(theta,61,41),type="l",lty=2)
lines(theta,dbeta(theta,63,43),type="l",lty=3,col="red")
legend("topright",legend=c("Prior","Likelihood","Posterior"),lty=c(1,2,3),col=c("black","black","red"),bty="n")
plot(theta,dbeta(theta,10,10),type="l",main="Moderate prior",ylim=c(0,10))
lines(theta,dbeta(theta,61,41),type="l",lty=2)
lines(theta,dbeta(theta,71,51),type="l",lty=3,col="red")
legend("topright",legend=c("Prior","Likelihood","Posterior"),lty=c(1,2,3),col=c("black","black","red"),bty="n")
plot(theta,dbeta(theta,20,20),type="l",main="Strong prior",ylim=c(0,10))
lines(theta,dbeta(theta,61,41),type="l",lty=2)
lines(theta,dbeta(theta,81,61),type="l",lty=3,col="red")
legend("topright",legend=c("Prior","Likelihood","Posterior"),lty=c(1,2,3),col=c("black","black","red"),bty="n")

