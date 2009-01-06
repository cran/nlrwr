## Chapter 2 in Nonlinear regression with R

L.minor

plot(rate~conc, data=L.minor, ylab="Uptake rate (weight/h)", xlab=Substrate~concentration~(mmol~m^-3))

## Fitting nonlinear regression
L.minor.m1 <- nls(rate ~ Vm*conc/(K+conc), data = L.minor, start = list(K=20, Vm=120), trace = TRUE)

## Extracting various information from the fit
deviance(L.minor.m1)

logLik(L.minor.m1)

coef(L.minor.m1)

summary(L.minor.m1)

fitted(L.minor.m1)

concVal <- with(L.minor, seq(min(conc), max(conc), length.out=10))
predict(L.minor.m1, data.frame(conc=concVal))

## Data and crude fitted curve
plot(rate ~ conc, data = L.minor, ylim=c(10,130), ylab="Uptake rate (weight/h)",
xlab=Substrate~concentration~(mmol~m^-3))

lines(L.minor$conc, fitted(L.minor.m1))

## Data and smooth fitted curve 
plot(rate ~ conc, data = L.minor, ylim=c(10,130), ylab="Uptake rate (weight/h)",
xlab=Substrate~concentration~(mmol~m^-3))

concVal <- with(L.minor, seq(min(conc), max(conc), length.out=100))

lines(concVal, predict(L.minor.m1, newdata=data.frame(conc=concVal)))

abline(h=coef(L.minor.m1)[2], lty = 2)

## Contours
L.minor.m1con <- nlsContourRSS(L.minor.m1)

plot(L.minor.m1con, col = FALSE, nlev = 10)

## GLM
L.minor.m4 <- glm(rate~I(1/conc), data=L.minor, family=gaussian("inverse"))

summary(L.minor.m4)



