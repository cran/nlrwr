## Chapter 3 in Nonlinear regression with R

library(NISTnls)
plot(y~x, data = Chwirut2,
xlab = "Metal distance",
ylab = "Ultrasonic response")

## Defining mean function
expFct <- function(x, beta1, beta2, beta3) {exp(-beta1*x)/(beta2+beta3*x)}

## Showing mean function for various parameter combinations 
plot(y~x, data = Chwirut2, xlab = "Metal distance", ylab = "Ultrasonic response", ylim=c(0,100))
curve(expFct(x, beta1=1, beta2=0.01, beta3=1), add = TRUE)

plot(y~x, data = Chwirut2, xlab = "Metal distance", ylab = "Ultrasonic response", ylim=c(0,100))
curve(expFct(x, beta1=0.1, beta2=0.01, beta3=1), add = TRUE, lty=2)
curve(expFct(x, beta1=0.1, beta2=0.01, beta3=0.1), add = TRUE, lty=3)
curve(expFct(x, beta1=0.1, beta2=0.01, beta3=0.01), add = TRUE, lty=4)
curve(expFct(x, beta1=0.2, beta2=0.01, beta3=0.01), add = TRUE, lty=1)

## Fitting nonlinear regression
Chwirut2.m1 <- nls(y ~ expFct(x, beta1, beta2, beta3), data=Chwirut2,
start=list(beta1=0.2,beta2=0.01,beta3=0.01))

summary(Chwirut2.m1)

## Grid search
grid.Chwirut2 <- expand.grid(list(beta1=seq(0.1, 1, by=0.1), beta2=c(0.01), beta3=seq(0.1, 1, by=0.1)))

Chwirut2.m2a <- nls2(y ~ expFct(x, beta1, beta2, beta3), data=Chwirut2, start = grid.Chwirut2, algorithm =
"brute-force")

Chwirut2.m2a

# Not shown in the book
Chwirut2.m2b <- nls2(y ~ expFct(x, beta1, beta2, beta3), data=Chwirut2, start=Chwirut2.m2a)

## Looking at self starters
L.minor.m2 <- nls(rate ~ SSmicmen(conc, Vm, K), data = L.minor)

summary(L.minor.m2)

expModel <- function(predictor, b, y0) {
    y0*exp(predictor/b)
}

expModelInit <- function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[["predictor"]], LHS, data)

    lmFit <- lm( log(xy[, "y"]) ~ xy[, "x"])
    coefs <- coef(lmFit)

    y0 <- exp(coefs[1])
    b <- 1/coefs[2]

    value <- c(b, y0)
    names(value) <- mCall[c("b", "y0")]
    value
}

SSexp <- selfStart(expModel, expModelInit, c("b", "y0"))
with(RGRcurve, SSexp(Day, 4, 0.2))

getInitial(RGR~SSexp(Day, b, y0), data=RGRcurve)

RGRcurve.m1 <- nls(RGR ~SSexp(Day, b, y0), data=RGRcurve)

coef(RGRcurve.m1)
