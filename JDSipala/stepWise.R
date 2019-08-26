houses <- read.csv(file=f, header=TRUE, sep=",")

#loading package
install.packages('caTools')
library(caTools)


#use caTools function to split, SplitRatio for 70%:30% splitting
houses1= sample.split(houses,SplitRatio = 0.3)

#subsetting into Train data
train =subset(houses,houses1==TRUE)

#subsetting into Test data
test =subset(houses,houses1==FALSE)


model.empty = lm(SalePrice ~ 1, data = train) #The model with an intercept ONLY.
model.full = lm(SalePrice ~ ., data = train) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))

summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)





