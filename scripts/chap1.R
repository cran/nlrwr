## Chapter 1 in Nonlinear regression with R

## Loading the support extension package
library(nlrwr)

## A stock-recruitment model
plot(num.fish~spawn.biomass, data=M.merluccius,
xlab="Spawning biomass (1000 tonnes)",
ylab="Recruitment (million fish)")

## Competition between plant biotypes
plot(biomass~x, data=RScompetition, log="",
xlab = Density~(plants/m^2),
ylab = Biomass~of~sensitive~biotype~(g/plant), pch=as.numeric(as.factor(RScompetition$z)))

## Grouped dose-response data
xyplot(DryMatter~Dose| Herbicide, data=S.alba, scales=list(x=list(log=TRUE)),
ylab="Dry matter (g/pot)", xlab="Dose (g/ha)")

