# install.packages("ISLR")

# (a) numerical and graphical summaries of the Weekly data
library(ISLR)
summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])

# (b) logistic regression with Direction as the response and the five lag variables plus Volume as predictors
glm.fit = glm(Direction~., data=Weekly[, c(2:7, 9)], family=binomial)
summary(glm.fit)

# (c) compute the confusion matrix and overall fraction of correct predictions
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Weekly$Direction)

# (d) fit the logistic regression model using a training data period from 1990 to 2008 with Lag2 as the only predictor
attach(Weekly)
train = (Year<2009)
Weekly.test = Weekly[!train,]
glm.fit = glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)
glm.probs = predict(glm.fit, Weekly.test, type="response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")
Direction.test = Direction[!train]
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)

# (e) linear discriminant analysis
library(MASS)
lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
lda.pred = predict(lda.fit, Weekly.test)
table(lda.pred$class, Direction.test)
mean(lda.pred$class==Direction.test)

# (f) kNN
library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
Direction.train = Direction[train]
set.seed(23)
knn.pred = knn(train.X, test.X, Direction.train, k=1)
table(knn.pred, Direction.test)
mean(knn.pred==Direction.test)

# (h) experiments
# logistic regression: Lag2+I(Lag1^2)
glm.fit = glm(Direction~Lag2+I(Lag1^2), data=Weekly, family=binomial, subset=train)
glm.probs = predict(glm.fit, Weekly.test, type="response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction.test)
mean(glm.pred==Direction.test)

# linear discriminant analysis: Lag2+I(Lag1^2)
lda.fit = lda(Direction~Lag2+I(Lag1^2), data=Weekly, subset=train)
lda.pred = predict(lda.fit, Weekly.test)
table(lda.pred$class, Direction.test)
mean(lda.pred$class==Direction.test)

# kNN: recursively increase k
k_increasement = data.frame(k=1:300, accuracy=NA)
for (i in 1:300){
  set.seed(23)
  knn.pred = knn(train.X, test.X, Direction.train, k=i)
  table(knn.pred, Direction.test)
  k_increasement$accuracy[i] = mean(knn.pred==Direction.test)
}
plot(x=k_increasement$k, y=k_increasement$accuracy, type='l', xlab="k", ylab="accuracy", ylim=c(0.45, 0.7), lwd=3, col='red')
identify(x=k_increasement$k, y=k_increasement$accuracy, plot=T)
k_increasement[154,]

