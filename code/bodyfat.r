library(dplyr)
library(ggplot2)
library(reshape2)
library(moments)
library(DataExplorer)
library(datasets)
library(tidyverse)
library(pairsD3)
library(AICcmodavg)
library(MASS)
library(Metrics)
library(caret)
theme_set(theme_classic())

#DATA CLEANING
data = read.csv("bodyfat.csv") 
attach(data) # This "attaches" the data into R. 

dim(data) # Dimension of the data (number of rows, number of columns)
colnames(data) #Variables in the Data
head(data) #Look at the first few data points 
tail(data) #Look at the last few data points

summary(data) #Gives you a brief summary statistic of all the variables in the data
# Note that the first column contains the index number of each individual.

# drop "DENSITY" because it is similar to "BODYFAT"
drop <- c("DENSITY")
data = data[,!(names(data) %in% drop)]

# EDA of dataset
plot_histogram(data)

plot_boxplot(data, by="BODYFAT")

shinypairs(data)

# Removing Outliers
for(col in colnames(data)){
  x<- data[,col]
  Q <- quantile(x, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(x)
  Lower <- Q[1] - 1.5*IQR
  Upper <- Q[2] + 1.5*IQR 
  BodyFat <- data %>% filter_at(vars(col),any_vars(.>Lower & .<Upper))
}
# save cleaned data set
write.table(BodyFat, file = "cleanedbodyfat.csv", sep=",")

# Correlation matrix
cormat <- round(cor(BodyFat),2)  #Correlation matrix can be created using the R function cor() :
melted_cormat <- melt(cormat)  #melt the correlation matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  #The function geom_tile() in ggplot2 package is used to visualize the correlation matrix 
  scale_fill_gradient2(low = "yellow", high = "black", mid = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1))+
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 2) 

# Multicollinearity is present in the data
BodyFat$group = ifelse(BodyFat$BODYFAT < 5, "0-5", ifelse(BodyFat$BODYFAT < 17, "6-17", ifelse(BodyFat$BODYFAT < 24, "18-24", ">25")))
# skewness
skewness(BodyFat)
# some columns are skewed-- HEIGHT, ADIPOSITY, ANKLE

# Selection of the significant predictors using general linear model
summary(bodyfat.mod <- lm(BODYFAT ~  AGE + NECK + KNEE + BICEPS + FOREARM + WRIST, data = BodyFat))

# stepwise
(lr.backward <- step(bodyfat.mod, scope = list(lower ~ Density), trace=0))

#MODELING
#Linear Regression
lr.mod1 <- lm(BODYFAT ~  WEIGHT + AGE + HIP + ABDOMEN , data = BodyFat)
lr.mod2 <- lm(BODYFAT ~  WEIGHT + THIGH + ABDOMEN , data = BodyFat)
lhr.mod3 <- rlm(BODYFAT ~  WEIGHT + ABDOMEN , data = BodyFat) #huber loss
lr.mod3 <- lm(BODYFAT ~  WEIGHT + ABDOMEN , data = BodyFat)
# Polynomial
p.mod1 <- lm(BODYFAT ~ WEIGHT + AGE + THIGH + ABDOMEN + I(ABDOMEN^2), data = BodyFat)
p.mod2 <- lm(BODYFAT ~ ABDOMEN + I(ABDOMEN^2), data = BodyFat)
p.mod3 <- lm(BODYFAT ~ WEIGHT + THIGH + ABDOMEN + I(ABDOMEN^2), data = BodyFat)
# Performance
summary(lr.mod2)
summary(lr.mod3)
#calculate AIC
models <- list(lr.mod1, lr.mod2, lr.mod3)
aictab(cand.set = models)

#Residual Analysis
par(mfrow = c(1,1))
plot(predict(lr.mod3), BodyFat$BODYFAT - predict(lr.mod3), ylim = c(-13, 13), xlim = c(5, 45), 
     pch = 3, col = 'red',xlab = 'Fitted value', ylab = 'Residual', main = 'Comparison of Residual Plots')
points(predict(lhr.mod3), BodyFat$BODYFAT - predict(lhr.mod3), ylim = c(-13, 13), xlim = c(5, 45), 
       pch = 18, col = 'blue', lwd= 2)
lowess1 = lowess(predict(lr.mod3), BodyFat$BODYFAT - predict(lr.mod3))
lowess2 = lowess(predict(lhr.mod3), BodyFat$BODYFAT - predict(lhr.mod3))
lines(lowess1$x, lowess1$y, col = 'green', lwd = 3)
lines(lowess2$x, lowess2$y, col = 'black', lwd = 2)

#Linearity
plot(predict(lr.mod3),resid(lr.mod3),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=3)

#Normality
qqnorm(rstandard(lr.mod3),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

#Leverage and Influential Points
pii = hatvalues(lr.mod3)
cooki = cooks.distance(lr.mod3)
n = dim(BodyFat)[1]
plot(1:n,pii,type="p",pch=19,cex=0.5,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Pii",main="Leverage Values (Pii)")
plot(1:n,cooki,type="p",pch=19,cex=0.5,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")

#Final Model
lr.final= function(WEIGHT, ABDOMEN) { 
  return(-44 + ABDOMEN -0.1*WEIGHT)
}
#Rule of thumb: BodyFat%= ABDOMEN-0.1*WEIGHT-44
pred_val = lr.final(BodyFat$WEIGHT, BodyFat$ABDOMEN)
plot(pred_val, BodyFat$BODYFAT - pred_val, col = 'blue', main = 'Residual Plots for Simplified Model',
     ylab = "Residual", xlab = "Fitted Value",pch=19,cex=0.5, ylim = c(-13, 9), xlim = c(2, 48))
lowess3 = lowess(pred_val, BodyFat$BODYFAT - pred_val)
lines(lowess3$x, lowess1$y, col = 'black', lwd = 2)

RMSE = RMSE(pred_val, BodyFat$BODYFAT)
R2 = R2(pred_val, BodyFat$BODYFAT)


