# (a) Generate a data set with p = 40 features, n = 2,500 observations, and an associated quantitative response vector generated according to the model Y = X?? + e, where ?? has some elements that are exactly equal to zero.
set.seed(23)
p = 40
n = 2500
x = matrix(rnorm(n*p), n, p)
b = rnorm(p)
b[5]=b[7]=b[11]=b[13]=b[17]=b[19]=b[29]=b[31]=b[37] = 0
e = rnorm(n)
y = x%*%b+e

# (b) Split your data set into 70% and 30% training and testing datasets respectively.
train = sample(seq(n*0.7), n*0.7, replace=FALSE)
y.train = y[train,]
y.test = y[-train,]
x.train = x[train,]
x.test = x[-train,]
data.train = data.frame(y=y.train, x=x.train)
data.test = data.frame(y=y.test, x=x.test)

# (c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter ??. Hint: use the gbm package, the gbm() function with the option distribution="Gaussian" to apply boosting to a regression problem.
# install.packages("gbm")
library(gbm)
set.seed(23)
pows = seq(-10, 0, 0.1)
lambdas = 10^pows
boost.test.err = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.data = gbm(y~., data=data.train, distribution="gaussian", n.trees=1000, shrinkage=lambdas[i])
  pred.boost.test = predict(boost.data, data.test, n.trees=1000)
  boost.test.err[i] = mean((pred.boost.test-y.test)^2)
}

# (d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
plot(lambdas, boost.test.err, type="b", col="red", lwd=2, xlab="Shrinkage Values", ylab="Test MSE")

# (e) Now apply bagging to the training set. What is the test set MSE for this approach?
# install.packages("randomForest")
library(randomForest)
set.seed(23)
bag.data = randomForest(y~., data=data.train, mtry=40, importance=TRUE)
pred.bag.test = predict(bag.data, newdata=data.test)
bag.test.err = mean((pred.bag.test-y.test)^2)
bag.test.err 

# (f) Now apply random forest to the training set. What is the test set MSE for this approach? Which variables appear to be the most important predictors in the random forest model?
set.seed(23)
rf.data = randomForest(y~., data=data.train, mtry=13, importance=TRUE)
pred.rf.test = predict(rf.data, newdata=data.test)
rf.test.err = mean((pred.rf.test-y.test)^2)
rf.test.err
importance(rf.data)
varImpPlot(rf.data)

# (g) Use the regsubsets() function of the leaps library to perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
# install.packages("leaps")
library(leaps)
set.seed(23)
regfit.train.err = rep(NA, 40)
regfit.data = regsubsets(y~., data=data.train, nvmax=40)
train.mat = model.matrix(y~., data=data.train, nvmax=40)
for (i in 1:40){
  coefi = coef(regfit.data, id=i)
  pred.regfit.train = train.mat[, names(coefi)] %*% coefi
  regfit.train.err[i] = mean((pred.regfit.train-y.train)^2)
}
plot(regfit.train.err, type="b", col="red", pch=19, lwd=2, xlab="Features in prediction", ylab="Training MSE")

# (h) Plot the test set MSE associated with the best model of each size.
set.seed(23)
regfit.test.err = rep(NA, 40)
test.mat = model.matrix(y~., data=data.test, nvmax=40)
for (i in 1:40){
  coefi = coef(regfit.data, id=i)
  pred.regfit.test = test.mat[, names(coefi)] %*% coefi
  regfit.test.err[i] = mean((pred.regfit.test-y.test)^2)
}
plot(regfit.test.err, type="b", col="red", pch=19, lwd=2, xlab="Features in prediction", ylab="Test MSE")

# (i) For which model size does the test set MSE take on its minimum value? Comment on your results. If it takes on its minimum value for a model containing only an intercept or a model containing all of the features, then play around with the way that you are generating the data in (a) until you come up with a scenario in which the test set MSE is minimized for an intermediate model size.
which.min(regfit.test.err)

# (j) How does the model at which the test set MSE is minimized compare to the true model used to generate the data? Comment on the coefficient values.
coef(regfit.data, id=29)

# (k) Create a plot displaying the square root of the sum of the squared difference between the coefficients of the estimated and the true model used to generate the data? How does this compare to the test MSE plot from (h)?
b.err = rep(NA, 20)
x.col = colnames(x, do.NULL=FALSE, prefix="x.")
for (i in 1:40) {
  coefi = coef(regfit.data, id=i)
  b.err[i] = sqrt(sum((b[x.col %in% names(coefi)]-coefi[names(coefi) %in% x.col])^2) + sum(b[!(x.col %in% names(coefi))])^2)
}
plot(b.err, type="b", col="red", pch=19, lwd=2, xlab="Features in prediction", ylab="beta error")
which.min(b.err)

# (l) Build a table to compare the test set MSE of your best model for:
# -Boosting
# -Bagging
# -Random Forests
# -Model selection by exhaustive search using the regsubsets() function.
# Discuss and explain your results.
table.errs = matrix(c(boost.test.err[which.min(boost.test.err)], bag.test.err, rf.test.err, regfit.test.err[which.min(regfit.test.err)]), ncol=4, byrow=TRUE)
colnames(table.errs) = c("Boosting", "Bagging", "Random Forest", "Model Selection")
rownames(table.errs) = c("MSE")
table.errs
