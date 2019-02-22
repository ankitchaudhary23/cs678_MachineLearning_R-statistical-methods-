library(ISLR)
library(MASS)
library(ggplot2)
library(datasets)
data(mtcars)

#9a
pairs(Auto)

#9b
cor(subset(Auto, select=-name))

#9c
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

### i.
#Yes, there is a relatioship between the predictors and the response by testing
#the null hypothesis of whether all the regression coefficients are zero. The F
#-statistic is far from 1 (with a small p-value), indicating evidence against
#the null hypothesis.

### ii.
#Looking at the p-values associated with each predictor's t-statistic, we see
#that displacement, weight, year, and origin have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.

### iii.
#The regression coefficient for year, ``r coefficients(lm.fit1)["year"]``,
#suggests that for every one year, mpg increases by the coefficient. In other
#words, cars become more fuel efficient every year by almost 1 mpg / year.


#9d
par(mfrow=c(2,2))
plot(lm.fit1)

#The fit does not appear to be accurate because there is a discernible curve
#pattern to the residuals plots. From the leverage plot, point 14 appears to have
#high leverage, although not a high magnitude residual.


plot(predict(lm.fit1), rstudent(lm.fit1))

#There are possible outliers as seen in the plot of studentized residuals
#because there are data with a value greater than 3.


#9e
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

#From the correlation matrix, I obtained the two highest correlated pairs and
#used them in picking my interaction effects. From the p-values, we can see
#that the interaction between displacement and weight is statistically
#signifcant, while the interactiion between cylinders and displacement is not.

#9f
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)

par(mfrow=c(2,2)) 
plot(lm.fit2)
plot(predict(lm.fit2),rstudent(lm.fit2))






# 13.a
set.seed(1)
x = rnorm(100)

# 13.b
eps = rnorm(100, 0, sqrt(0.25))

# 13.c
y = -1 + 0.5*x + eps     # y is of length 100. $\beta_0$ is -1, $\beta_1$ is 0.5.

# 13.d
plot(x, y)    # I observe a linear relationship between x and y with a positive slope, with a variance as is to be expected.


# 13.e
lm.fit = lm(y~x)
summary(lm.fit)
# [The linear regression fits a model close to the true value of the coefficients 
#   as was constructed. The model has a large F-statistic with a near-zero p-value
#   so the null hypothesis can be rejected.]

# 13.f
plot(x, y)
abline(lm.fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

#13.g
lm.fit_sq = lm(y~x+I(x^2))
summary(lm.fit_sq)
# There is evidence that model fit has increased over the training data given the
#  slight increase in $R^2$ and $RSE$. Although, the p-value of the t-statistic
#  suggests that there isn't a relationship between y and $x^2$.


#13.h
set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)
abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
# As expected, the error observed in $R^2$ and $RSE$ decreases considerably.

#13.i
set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)
abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
# As expected, the error observed in $R^2$ and $RSE$ increases considerably.

#13.j
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)
# All intervals seem to be centered on approximately 0.5, with the second fit's
#  interval being narrower than the first fit's interval and the last fit's
#  interval being wider than the first fit's interval.