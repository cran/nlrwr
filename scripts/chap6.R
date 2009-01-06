## Chapter 6 in Nonlinear regression with R

## Power of mean
plot(RGR~Day, data=RGRcurve, xlab = "Time (days)", ylab = "Relative growth rate (%)" )

RGRcurve.m2 <- gnls(RGR ~ SSexp(Day, a, b), data=RGRcurve, weights=varPower())

summary(RGRcurve.m2)


## Transform-both-sides
plot(recruits~spawners, data=sockeye[-12,], xlab="Number of spawners (thousands)",
ylab="Number of recruits (thousands)")

sockeye.m1 <- nls(recruits ~ beta1 * spawners * exp(-beta2 * spawners), data = sockeye[-12, ], start =
list(beta1 = 2, beta2 = 0.001))

plot(fitted(sockeye.m1), abs(residuals(sockeye.m1)), xlab="Fitted values", ylab="Absolute residuals")

sockeye.m2 <- boxcox.nls(sockeye.m1)

bcSummary(sockeye.m2)

coef(summary(sockeye.m1))
coef(summary(sockeye.m2))

vcov(sockeye.m1)

sandwich(sockeye.m1)

coeftest(sockeye.m1)
coeftest(sockeye.m1, vcov=sandwich)

## Weights

# Dataset exp1
exp1
plot(Nremaining~time, data=exp1, xlab="Time (years)", ylab="Nitrogen content (%)")

exp1.m1<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp1)
exp1.m2<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp1, weights=norep/(stdev*stdev))

weights(exp1.m2)

coef(summary(exp1.m1))
coef(summary(exp1.m2))

# Dataset exp2
exp2

exp2.m1<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp2)
exp2.m2<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp2, weights=norep/(stdev^2))

plot(Nremaining~time, data=exp2, xlab="Time (years)", ylab="Nitrogen content (%)")

timeVal <- with(exp2, seq(min(time), max(time), length.out=100))
lines(timeVal, predict(exp2.m1, newdata=data.frame(time=timeVal)), lty=2)
lines(timeVal, predict(exp2.m2, newdata=data.frame(time=timeVal)), lty=3)

coef(summary(exp2.m1))
coef(summary(exp2.m2))

