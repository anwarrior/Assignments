#Binomial distribution approaches Poisson
#generating value for binomial (number_trials-->inf & prob-->0)
binom1 = rbinom(300,100, 0.053)
binom2 = rbinom(300,200, 0.0265)
binom3 = rbinom(300,300, 0.01766667)
#Generating value for poisson (lambda = 5.3)
pois1 = rpois(300,5.3)

#Frequency Distribution binomial
cbind.data.frame(table(binom1))
cbind.data.frame(table(binom2))
cbind.data.frame(table(binom3))

#Frequency Distribution Poisson
cbind.data.frame(table(pois1))

#Using par to combine graphs together
par(mfrow=c(2,2))

#Plotting 4 graphs
m1 = mean(binom1)
std1 = sqrt(var(binom1))
hist(binom1, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.7), 
     main="Binomial (100)", col = "darkgoldenrod1")
curve(dnorm(x, mean=m1, sd=std1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n") 

m2 = mean(binom2)
std2 = sqrt(var(binom2))

hist(binom2, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.7), 
     main="Binomial (200)", col = "darkgoldenrod1")
curve(dnorm(x, mean=m2, sd=std2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

m3 = mean(binom3)
std3 = sqrt(var(binom3))
hist(binom3, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.7), 
     main="Binomial (300)", col = "darkgoldenrod1")
curve(dnorm(x, mean=m3, sd=std3), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

mp = mean(pois1)
stdp = sqrt(var(pois1))
hist(pois1, density=20, breaks=20, prob=TRUE, 
     xlab="x_Poisson", ylim=c(0, 0.7), 
     main="Poisson Distribution", col = "darkgreen")
curve(dnorm(x, mean=mp, sd=stdp), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")



