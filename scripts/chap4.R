## Chapter 4 in Nonlinear regression with R

## Using derivatives

# Manual
MMfct1 <- function(conc, K, Vm) {
    ## Block 1
    numer <- Vm*conc
    denom <- K+conc
    mean <- numer/denom

    ## Block 2
    partialK <- -numer/(denom^2)
    partialVm <- mean/Vm

    ## Block 3
    attr(mean, "gradient") <- cbind(partialK, partialVm)
    return(mean)
}

L.minor.mgr1 <- nls(rate~MMfct1(conc, K, Vm), data=L.minor, start=list(K=20, Vm=120))
summary(L.minor.mgr1)

# Automatic
MMfct2 <- deriv(~Vm*conc/(K+conc), c("K", "Vm"), function(conc, K, Vm){})

L.minor.mgr2 <- nls(rate~MMfct2(conc, K, Vm), data=L.minor, start=list(K=20, Vm=120))

## Using "plinear"
L.minor.m3 <- nls(rate~conc/(K+conc), data=L.minor, algorithm="plinear", start=list(K=20))

summary(L.minor.m3)

## Pedestrian approach
data(segreg)

plot(C~Temp, data=segreg,
xlab=Mean~temperature~(degree~F),
ylab="Energy consumption (kWh)")

profRSS1<-function(gamma){deviance(lm(C~pmax(0, Temp-gamma), data=segreg))}
profRSS2<-Vectorize(profRSS1, "gamma")

plot(profRSS2(Temp)~Temp, data=segreg, type="l", xlab=expression(gamma), ylab="Profile RSS")

## 2-dim predictor
RScompetition.m1 <- nls(biomass~a/(1+b*(x+c*z)), data=RScompetition, start=list(a=20, b=1, c=1))

summary(RScompetition.m1)

virDensity <- with(RScompetition, x + coef(RScompetition.m1)[3]*z)
virDenVal <- seq(0, max(virDensity), length.out = 100)
biomassVal <- predict(RScompetition.m1, data.frame(x=virDenVal, z=0))

plot(biomassVal ~ virDenVal, type = "l",
ylab="Biomass of sensitive biotype (g/plant)",
xlab=Virtual~density~(plants/m^2))

with(RScompetition, points(biomass~virDensity))

## General least squares
plot(Q~I, data = IQsig)

theta <- 0:360*(pi/180)
lines(cos(theta), sin(theta))

IQsig.m1 <- nls(~((I-I0)^2-2*gamma*sin(phi)*(I-I0)*(Q-Q0)+gamma*gamma*(Q-Q0)^2)-(rho*gamma*cos(phi))^2, data =
IQsig, start = list(I0=-0.005,gamma=1,phi=-0.005,Q0=-0.005,rho=1))

summary(IQsig.m1)

## Controlling nls()
nls.control()

