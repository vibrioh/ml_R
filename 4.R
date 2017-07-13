# install.packages('tree')

# (a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.
library(tree)
library(ISLR)
set.seed(2388)
train = sample(1:nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

# (b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics about the tree, and describe the results obtained. What is the training error rate? How many terminal nodes does the tree have?
tree.oj = tree(Purchase~., data=OJ.train)
summary(tree.oj)

# (c) Type in the name of the tree object in order to get a detailed text output. 
tree.oj

# (d) Create a plot of the tree.
plot(tree.oj)
text(tree.oj, pretty=0)

# (e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels.
tree.pred = predict(tree.oj, OJ.test, type="class")
table(tree.pred, OJ.test$Purchase)
mean(tree.pred!=OJ.test$Purchase)

# (f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
cv.oj = cv.tree(tree.oj, FUN=prune.misclass)
cv.oj

# (g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
plot(cv.oj$size, cv.oj$dev, type='b', col="red", ylim=c(160, 315), lwd=3, xlab='size', ylab='cross-validated classification error rate')

# (i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation.
prune.oj = prune.misclass(tree.oj, best=4)
plot(prune.oj)
text(prune.oj, pretty=0)

# (j) Compare the training and test error rates between the pruned and unpruned trees.
summary(prune.oj)
prune.pred = predict(prune.oj, OJ.test, type="class")
table(prune.pred, OJ.test$Purchase)
mean(prune.pred!=OJ.test$Purchase)

