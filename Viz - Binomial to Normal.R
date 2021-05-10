#Binomial distribution Approaches normal
#np > 5
#nq > 5
#observation ---> inf
#Using par to combine 9 graphs together 
par(mfrow = c(3,3))

#ROW 1
n1 = 100
p1 = 0.053
q1 = 1-p1
mn1 = n1*p1
stdn1 = sqrt(n1*p1*q1)
norm11 = rbinom(300,n1, p1 )
norm12 = rbinom(600,n1, p1 )
norm13 = rbinom(1000,n1, p1 )

#ROW 2
n2 = 200
p2 = 0.0265
q2 = 1-p2
mn2 = n2*p2
stdn2 = sqrt(n2*p2*q2)
norm21 = rbinom(300,n2,p2 )
norm22 = rbinom(600,n2,p2 )
norm23 = rbinom(1000,n2,p2 )

#ROW 3
n3 = 300
p3 = 0.01766667
q3 = 1-p3
mn3 = n3*p3
stdn3 = sqrt(n3*p3*q3)
norm31 = rbinom(300,n3, p3)
norm32 = rbinom(600,n3, p3)
norm33 = rbinom(1000,n3, p3)

#Plotting ROW 1
#(1,1)
hist(norm11, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.4), 
     main="Normal (Obs = 300, Trials = 100)", col = "green")
curve(dnorm(x, mean=mn1, sd=stdn1), 
      col="darkblue", lwd = 2 , add=TRUE, yaxt="n")

#(1,2)
hist(norm12, density=20, breaks=20, prob=TRUE, 
     xlab="X_binom", ylim=c(0, 0.6), 
     main="Normal (Obs = 600, Trials = 100)", col = "green")
curve(dnorm(x, mean=mn1, sd=stdn1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#(1,3)
hist(norm13, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.6), 
     main="Normal (Obs = 1000, Trials = 100)", col = "green")
curve(dnorm(x, mean=mn1, sd=stdn1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#plotting ROW 2
#(2,1)
hist(norm21, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.7), 
     main="Normal (Obs = 300, Trials = 200)", col = "green")
curve(dnorm(x, mean=mn2, sd=stdn2), 
      col="darkblue", lwd = 2 , add=TRUE, yaxt="n")

#(2,2)
hist(norm22, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.6), 
     main="Normal (Obs = 600, Trials = 200)", col = "green")
curve(dnorm(x, mean=mn2, sd=stdn2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#(2,3)
hist(norm23, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.6), 
     main="Normal (Obs = 1000, Trials = 200)", col = "green")
curve(dnorm(x, mean=mn2, sd=stdn2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#Plotting ROW 3
#(3,1)
hist(norm31, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.7), 
     main="Normal (Obs = 300, Trials = 300)", col = "green")
curve(dnorm(x, mean=mn3, sd=stdn3), 
      col="darkblue", lwd = 2 , add=TRUE, yaxt="n")

#(3,2)
hist(norm32, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.4), 
     main="Normal (Obs = 600, Trials = 300)", col = "green")
curve(dnorm(x, mean=mn3, sd=stdn3), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

#(3,3)
hist(norm33, density=20, breaks=20, prob=TRUE, 
     xlab="X_Binom", ylim=c(0, 0.4), 
     main="Normal (Obs = 1000, Trials = 300)", col = "green")
curve(dnorm(x, mean=mn3, sd=stdn3), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")