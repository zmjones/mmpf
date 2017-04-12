set.seed(1987)

library(mmpf)
library(randomForest)
library(data.table)
library(ggplot2)
library(mvtnorm)

## iris demonstration example
data(iris)
fit = randomForest(Species ~ ., data = iris)
iris.features = iris[, -ncol(iris)]

mp = marginalPrediction(data = iris.features,
  vars = "Petal.Width",
  n = c(10, nrow(iris)), model = fit, uniform = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "prob"))

plt = melt(mp, id.vars = "Petal.Width",
  variable.name = "Class", value.name = "Marginal Expected Value")
ggplot(plt, aes(Petal.Width, `Marginal Expected Value`, color = Class)) +
  geom_line() + geom_point()
ggsave("mp.png", width = 8, height = 4)

mp.int = marginalPrediction(data = iris.features,
  vars = c("Petal.Width", "Petal.Length"),
  n = c(10, nrow(iris)), model = fit, uniform = TRUE,
  predict.fun = function(object, newdata) predict(object, newdata, type = "prob"),
  aggregate.fun = function(x) list("mean" = mean(x), "variance" = var(x)))

mp.int = melt(mp.int, id.vars = c("Petal.Width", "Petal.Length"))
mp.int[, c("class", "fun") := tstrsplit(variable, "\\.")]

ggplot(mp.int[fun == "mean"], aes(Petal.Width, Petal.Length)) +
  geom_raster(aes(fill = value)) + facet_wrap(~ class) +
  scale_fill_gradient2(name = "Marginal\nProbability", midpoint = .5)
ggsave("mp_int_mean.png", width = 8, height = 4)

ggplot(mp.int[fun == "variance"], aes(Petal.Width, Petal.Length)) +
  geom_raster(aes(fill = value)) + facet_wrap(~ class) +
  scale_fill_gradient(name = "Marginal\nVariance")
ggsave("mp_int_var.png", width = 8, height = 4)

## marginalization example
mp = function(n, sigma, f) {
  aggregate.fun = function(x, w) {
    mu = weighted.mean(x, w)
    sigma = (sum(w * x^2) * sum(w) - sum(w * x)^2) / (sum(w)^2 - sum(w^2))
    list(
      "mean" = mu,
      "lower" = mu + qnorm(.025) * sigma,
      "upper" = mu + qnorm(.975) * sigma)
  }

  points = list("X1" = seq(-4, 4, length.out = 100))
  p.joint = function(design, data, sigma) dmvnorm(design[, c("X1", "X2")], sigma = sigma)
  formals(p.joint)$sigma = sigma
  p.marginal = function(design, data, sigma) dnorm(design$X2)

  X = rmvnorm(n, sigma = sigma)
  data = data.frame(y = f(NULL, X), X)
  list(
    "joint" = marginalPrediction(data[, c("X1", "X2")], "X1", c(25, n), NULL,
      points = points, weight.fun = p.joint, predict.fun = f,
      aggregate.fun = aggregate.fun),
    "marginal" = marginalPrediction(data[, c("X1", "X2")], "X1", c(25, n), NULL,
      points = points, weight.fun = p.marginal, predict.fun = f,
      aggregate.fun = aggregate.fun)
  )
}

n = 10000
sigma.diagonal = diag(2)
sigma.dependent = matrix(c(1, .5, .5, 1), 2, 2)

X = data.frame(rmvnorm(500, sigma = sigma.dependent))
ggplot(X, aes(X1, X2)) + 
  geom_point()
ggsave("joint.png", width = 7, height = 6)

f.additive = function(object, newdata) as.numeric(as.matrix(newdata) %*% c(1, 1))
f.interaction = function(object, newdata)
  as.numeric(as.matrix(data.frame(newdata, newdata[, 1] * newdata[, 2])) %*% c(1, 1, .5))

plt = list(
  "additive" = mp(n, sigma.dependent, f.additive),
  "interaction" = mp(n, sigma.dependent, f.interaction)
)

plt = rbindlist(lapply(plt, rbindlist, idcol = "estimation"), idcol = "sim.type")

ggplot(plt, aes(X1, preds.mean, color = estimation)) +
  geom_line() +
  facet_wrap(~ sim.type) +
  labs(y = expression(f(X[1])), x = expression(X[1])) +
  geom_line(aes(X1, X1), linetype = "dashed", color = "black")
ggsave("mvj.png", width = 8, height = 4)
