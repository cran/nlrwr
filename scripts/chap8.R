## Chapter 8 in Nonlinear regression with R

## Fitting grouped data models
xyplot(rate~conc|state, data=Puromycin, xlab="Substrate concentration (ppm)", ylab="Reaction rates
(counts/min/min)")

## Approach 1
Puromycin.m1 <- nls(rate ~ Vm[state]*conc/(K[state]+conc), data=Puromycin, start=list(K=c(0.1,0.1),
Vm=c(200,200)))

summary(Puromycin.m1)

## Approach 2
Puromycin.m2 <- gnls(rate ~ Vm*conc/(K+conc), data=Puromycin, start=list(Vm=c(200,200), K=c(0.1,0.1)),
params=list(Vm~state-1, K~state-1))

summary(Puromycin.m2)

## Approach 3
Puromycin.m3 <- nlsList(rate ~ SSmicmen(conc, Vm, K)|state, data=Puromycin)

Puromycin2 <- groupedData(rate ~ conc|state, data=Puromycin)

Puromycin.m4 <- nlsList(rate ~ SSmicmen(conc, a, b), data=Puromycin2)

## Comparison of entire groups
Puromycin.m5 <- nls(rate ~ Vm*conc/(K+conc), data=Puromycin, start=list(K=0.1, Vm=200))

anova(Puromycin.m5, Puromycin.m1)

## Comparison of specific parameters
Puromycin.m6 <- nls(rate ~ Vm*conc/(K[state]+conc), data=Puromycin, start=list(K=c(0.1,0.1), Vm=200))

anova(Puromycin.m6, Puromycin.m1)

Puromycin.m7 <- nls(rate ~ Vm[state]*conc/(K+conc), data=Puromycin, start=list(K=0.1, Vm=c(200,200)))

anova(Puromycin.m7, Puromycin.m1)

summary(Puromycin.m7)

## Common control
xyplot(drymatter~dose|as.factor(treatment), data=G.aparine, xlab="Dose (g/ha)", ylab="Dry matter (mg/pot)")

G.aparine$treatment2 <- factor(G.aparine$treatment)

levels(G.aparine$treatment2)<-c("1","1","2")

G.aparine.m1<-nls(drymatter~c[treatment2]+(d-c[treatment2])/(1+exp(b[treatment2]*(log(dose)-log(e[treatment2])))),
data=G.aparine, start=list(b=c(2, 2),c=c(500,100),d=1000,e=c(50,100)))

summary(G.aparine.m1)

## Prediction
concValues <- with(Puromycin, seq(min(conc), max(conc), length.out = 10))
concValues

stateVal1 <- levels(Puromycin$state)
stateVal1

csValues1 <- expand.grid(conc = concValues, state = stateVal1)
csValues1

predict(Puromycin.m7, csValues1)

stateVal2 <- factor("untreated", levels = c("treated","untreated"))
stateVal2

csValues2 <- data.frame(conc = concValues, state = stateVal2)
csValues2

predict(Puromycin.m1, csValues2)

## Nonlinear mixed models
xyplot(effect~conc|exper, data=vinclozolin, xlab=expression(paste("Concentration (", mu, "M)")),
ylab="Luminescence (LU)")

LL3.formula <- effect ~ d/(1+exp(b*(log(conc)-log(e))))

vinclozolin.e1.m <- nls(LL3.formula, data=vinclozolin, subset = exper==10509, start=list(b=1,d=1000,e=0.26))

vinclozolin.e2.m <- nls(LL3.formula, data=vinclozolin, subset = exper==10821, start=list(b=1,d=1200,e=0.074))

vinclozolin.e3.m <- nls(LL3.formula, data=vinclozolin, subset = exper==10828, start=list(b=1,d=2800,e=0.15))

vinclozolin.e4.m <- nls(LL3.formula, data=vinclozolin, subset = exper==10904, start=list(b=1,d=2700,e=0.03))

vinclozolin.e5.m <- nls(LL3.formula, data=vinclozolin, subset = exper==11023, start=list(b=1,d=1400,e=0.14))

vinclozolin.e6.m <- nls(LL3.formula, data=vinclozolin, subset = exper==11106, start=list(b=0.5,d=2600,e=0.02))

plot(effect~conc, data=vinclozolin, pch=as.numeric(exper), log="x", xlim=c(0.0001, 10),
xlab=expression(paste("Concentration (", mu, "M)")), ylab="Luminescence (LU)")

concVec <- exp(seq(log(0.0001), log(10), length.out = 50))
lines(concVec, predict(vinclozolin.e1.m, data.frame(conc = concVec)), lty = 2)
lines(concVec, predict(vinclozolin.e2.m, data.frame(conc = concVec)), lty = 3)
lines(concVec, predict(vinclozolin.e3.m, data.frame(conc = concVec)), lty = 4)
lines(concVec, predict(vinclozolin.e4.m, data.frame(conc = concVec)), lty = 5)
lines(concVec, predict(vinclozolin.e5.m, data.frame(conc = concVec)), lty = 6)
lines(concVec, predict(vinclozolin.e6.m, data.frame(conc = concVec)), lty = "3313")

vinclozolin.m1 <- nlme(effect ~ d/(1+exp(b*(log(conc)-log(e)))), fixed = list(b~1, d~1, e~1),random = d~1|exper,
start = c(1, 1000, 0.1), data = vinclozolin)

lines(concVec, predict(vinclozolin.m1, data.frame(conc = concVec), level = 0), lty = 1, lwd = 3)

summary(vinclozolin.m1)
