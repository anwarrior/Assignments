##Question 2

#install.packages("TSA")
library(TSA)
data(wages)
wages
View(wages)
plot(wages, type='o', ylab='wages per hour')

wages.lm = lm(wages~time(wages))
summary(wages.lm) #r square seems perfect
plot(wages.lm)
plot(y=rstandard(wages.lm), x=as.vector(time(wages)), type = 'o')

Wages_Additive <- decompose(wages, type = "additive")
plot(Wages_Additive)


Wages_Multiplicative <- decompose(wages, type="mult") # use type = "additive" for additive components
plot (Wages_Multiplicative)

##Question 4
BJsales
sales <- ts(BJsales, start = c(2000, 1), frequency =12)
# Time vs Demand
plot(sales)

# Filtering
hw <- HoltWinters(sales)
plot(hw)

# For alpha,beta and gamma
hw ; hw$coef ; hw$SSE

#graphical visualization of level,trend,season
plot(hw$fitted)


sqrt(hw$SSE/length(hw))
sd(sales)

