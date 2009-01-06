"SSexp" <- function (predictor, y0, b) 
{
    y0 * exp(predictor/b)
}

attr(SSexp, "initial") <- function (mCall, LHS, data) 
{
    xy <- sortedXyData(mCall[["predictor"]], LHS, data)
    lmFit <- lm(log(xy[, "y"]) ~ xy[, "x"])
    coefs <- coef(lmFit)
    y0 <- exp(coefs[1])
    b <- 1/coefs[2]
    value <- c(y0, b)
    names(value) <- mCall[c("y0", "b")]
    value
}

attr(SSexp, "pnames") <- c("y0", "b") 

attr(SSexp, "class") <- "selfStart"
