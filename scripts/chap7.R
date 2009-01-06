## Chapter 7 in Nonlinear regression with R

## Profiling
L.minor.m1 <- update(L.minor.m1, trace = FALSE)
L.minor.m1pro <- profile(L.minor.m1)

plot(L.minor.m1pro)

plot(L.minor.m1pro, absVal = FALSE)

confint(L.minor.m1)

## Bootstrap
L.minor.m1boot <- nlsBoot(L.minor.m1)

qqnorm(L.minor.m1boot$coefboot[, 1], main="K")
qqnorm(L.minor.m1boot$coefboot[, 2], main="Vm")

summary(L.minor.m1boot)

## Wald
confint2(L.minor.m1)
confint2(L.minor.m1, level=0.99)

delta.method(L.minor.m1, "Vm/(4*K)")

## Nested models
secalonic

plot(rootl~dose, data=secalonic, xlab="Dose (mM)", ylab="Root length (cm)")

secalonic.m1<-nls(rootl~SSfpl(dose, a,b,c,d), data=secalonic)

summary(secalonic.m1)

secalonic.m2<-nls(rootl~SSlogis(dose, a,c,d), data=secalonic)

anova(secalonic.m2, secalonic.m1)

## Non-nested

# Beverton-Holt
M.merluccius.bh<-nls(num.fish~spawn.biomass*alpha/(1+spawn.biomass/k), data=M.merluccius,
start=list(alpha=5,k=50))

# Deriso
M.merluccius.de<-nls(num.fish~spawn.biomass*alpha*(1-c*spawn.biomass/k)^(1/c), data=M.merluccius,
start=list(alpha=4.4,k=106,c=0.86))

# Ricker
M.merluccius.ri<-nls(num.fish~spawn.biomass*alpha*exp(-spawn.biomass/k), data=M.merluccius,
start=list(alpha=5,k=50))

# Shepherd
M.merluccius.sh<-nls(num.fish~spawn.biomass*alpha/(1+(spawn.biomass/k)^c), data=M.merluccius,
start=list(alpha=3.87,k=61.72,c=2.25),control=nls.control(maxiter=100))

summary(M.merluccius.bh)$sigma
summary(M.merluccius.de)$sigma
summary(M.merluccius.ri)$sigma
summary(M.merluccius.sh)$sigma

AIC(M.merluccius.bh)
AIC(M.merluccius.de)
AIC(M.merluccius.ri)
AIC(M.merluccius.sh)