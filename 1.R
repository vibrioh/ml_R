# (a) medv.mean
library(MASS)
attach(Boston)
mu.hat = mean(medv)
mu.hat

# (b) std.error of medv.mean 
sem.mu.hat = sd(medv)/sqrt(length(medv))
sem.mu.hat

# (c) sem by bootstrap
library(boot)
set.seed(1)
boot.fn = function(data, index){
  return(mean(data[index]))
}
boot(medv, boot.fn, 1000)

# (d) confidence intervals
ci.bootstrap = c(22.53281-2*0.4119374, 22.53281+2*0.4119374)
ci.bootstrap
t.test(medv)

# (e) medv.median
med.hat = median(medv)
med.hat

# (f) std.error of medv.median
boot.fn = function(data, index){
  return(median(data[index]))
}
boot(medv, boot.fn, 1000)

# (g) 10 percentile
pct.hat = quantile(medv, c(0.1))
pct.hat

# (h) std.error of 10 percentile
boot.fn = function(data, index){
  return(quantile(data[index], c(0.1)))
}
boot(medv, boot.fn, 1000)
