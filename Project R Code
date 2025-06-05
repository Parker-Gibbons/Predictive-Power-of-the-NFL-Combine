library(mosaic)
library(leaps)
library(HH)
library(MASS)
library(glmnet)
library(ridge)

#Import data
combine <- read.csv("~/Downloads/Combine.csv")

#EDA
numbo <- read.csv("~/Downloads/numbo.csv") #only numeric variables
cor(numbo) #correlation matrix

#Full models
avmodel=lm(Average~Ht+Wt+X40yd+Vertical+Bench+Broad.Jump+X3Cone+Shuttle+as.factor(Drafted.), data=combine, x=TRUE)
tomodel=lm(Total.Yards.from.Scrimmage~Ht+Wt+X40yd+Vertical+Bench+Broad.Jump+X3Cone+Shuttle+as.factor(Drafted.), data=combine, x=TRUE)

#Model selection and diagnostics

##Average yards
summary(avmodel)
anova(avmodel)
vif(avmodel)

MSE = (summary(avmodel)$sigma)^2
step(avmodel, scale=MSE, direction="backward")
avback=lm(Average~Ht+Wt+as.factor(Drafted.), data=combine)
summary(avback)

finav=lm(Average~Ht+Wt++as.factor(Drafted.), data=combine, x=TRUE)
summary(finav)
anova(finav)
plot(finav)
vif(finav)

##Total yards
summary(tomodel)
anova(tomodel)
plot(tomodel)
vif(tomodel)

MSE = (summary(avmodel)$sigma)^2
step(tomodel, scale=MSE, direction="backward")
toback=lm(Total.Yards.from.Scrimmage~Ht+Wt+X40yd+Vertical+as.factor(Drafted.),data=combine)
summary(toback)

finto=lm(Total.Yards.from.Scrimmage~Ht+Wt+Vertical+as.factor(Drafted.),data=combine, x=TRUE)
summary(finto)
plot(finto)
vif(finto)

#Ridge Regression
predictor_matrix <- as.matrix(combine)
install.packages("glmnet")
library(glmnet)
x <- model.matrix(~ Ht + Wt + X40yd + Vertical + Bench + Broad.Jump + X3Cone + Shuttle + as.factor(Drafted.) -1, data = combine)
y <- combine$Average
X = x
Y = y
ridge <- glmnet(x = X, y = Y, alpha = 0)

plot(ridge, xvar = "lambda")
ridge2 <- cv.glmnet(x = X, y = Y, alpha = 1, nfolds = 60)
plot(ridge2)
best_ridge <-glmnet(x = X, y = Y, alpha = 0, lambda = 8.97)
coef(best_ridge)
prediction2<- predict(best_ridge, s = 8.97, newx = X)
sst <- sum((Y - mean(Y))^2)
sse <- sum((prediction2 - Y)^2)
rsq <- 1 - sse/sst


