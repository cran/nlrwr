## Chapter 5 in Nonlinear regression with R

## Plots
plot(p~T, data = vapCO, log="y", xlab="Temperature (K)", ylab="Pressure (Pa)")

vapCO.m1<-nls(log(p)~A-B/(C+T), data=vapCO, start=list(A=10,B=100,C=-10))

plot(p~T, data = vapCO, log="y", xlab="Temperature (K)", ylab="Pressure (Pa)")
lines(vapCO$T, exp(fitted(vapCO.m1)))

plot(weight~conc, data = lettuce, xlab="Concentration (mg/l)", ylab="Biomass (g)", log = "x")

## Residual plots
plot(fitted(vapCO.m1), residuals(vapCO.m1), xlab="Fitted Values", ylab="Residuals")

abline(a=0, b=0)

## Lack-of-fit test
plot(rootl~conc, data=ryegrass, xlab="Concentration (mM)", ylab="Root length (cm)")

ryegrass.m1 <- lm(rootl~as.factor(conc), data = ryegrass)

ryegrass.m2 <- nls(rootl ~ c + (d - c)/(1 + exp(b * + (log(conc) - log(e)))), start = list(b = 1, c = 0.6, d =8,
e = 3), data = ryegrass)

anova(ryegrass.m2, ryegrass.m1)

Q <- -2*(logLik(ryegrass.m2) - logLik(ryegrass.m1))
df.Q <- df.residual(ryegrass.m2) - df.residual(ryegrass.m1)
1-pchisq(Q, df.Q)

## Variance homogeneity
plot(fitted(vapCO.m1), abs(residuals(vapCO.m1)), xlab="Fitted values", ylab="Absolute residuals")

with(ryegrass, levene.test(rootl, as.factor(conc)))

## Normal distribution
plot(vapCO.m1)

standardRes <- residuals(ryegrass.m2) / summary(ryegrass.m2)$"sigma"
qqnorm(standardRes, main = "")
abline(a = 0, b = 1)

shapiro.test(standardRes)

plot(residuals(vapCO.m1), c(residuals(vapCO.m1)[-1], NA), xlab="Residuals", ylab="Lagged residuals")



