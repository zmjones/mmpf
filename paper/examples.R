library(mmpf)
library(randomForest)
library(e1071)
library(data.table)
library(ggplot2)
library(ddalpha)

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

X = replicate(2, runif(100))
w = c(1, 1)
data = data.frame(y = X %*% w, X)
data = data[!(data$X1 > .5 & data$X2 > .5), ]

ggplot(data, aes(X1, X2)) + geom_point()
ggsave("mp_w_data.png", width = 8, height = 4)

## fit = svm(y ~ ., data, kernel = "linear")
fit = NULL

predict.fun = function(object, newdata) {
  idx = newdata$X1 > .5 & newdata$X2 > .5
  preds = vector("numeric", nrow(newdata))
  preds = as.matrix(newdata) %*% w
  preds[idx] = preds[idx] + 10 * ((newdata[idx, "X1"] - 1)^2  * (newdata[idx, "X2"] - 1)^2)
  unname(preds)
}

## design = makeDesign(data, "X1", c(10, nrow(data)))

## ggplot(design, aes(X1, X2)) + geom_point()

## data = rbind(data, c("y" = 1.5^2 + .5, "X1" = 1.5, "X2" = .5))

ggplot(data, aes(X1, X2)) +
  ## stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  ## geom_vline(aes(xintercept = 1), linetype = "dashed") +
  geom_point()
ggsave("mp_w_data.png", width = 8, height = 4)

mp.uw = marginalPrediction(data = data[, -1], vars = "X1",
  n = c(10, nrow(data)), model = fit, uniform = TRUE,
  predict.fun = predict.fun)

mp.w = marginalPrediction(data = data[, -1], vars = "X1",
  n = c(10, nrow(data)), model = fit, uniform = TRUE,
  predict.fun = predict.fun,
  aggregate.fun = weighted.mean,
  weight.fun = function(design, data) ifelse(design$X1 > .5 & design$X2 > .5, 0, 1))

plt = data.frame("weighted" = mp.w[, 2],
  "unweighted" = mp.uw[, 2],
  "X1" = mp.w$X1,
  "truth" = mp.w$X1 * w[1] + w[2] * mean(data$X2))

ggplot(plt, aes(X1)) +
  geom_line(aes(X1, truth), color = "black") +
  geom_line(aes(X1, weighted), color = "blue") +
  geom_line(aes(X1, unweighted), color = "red") +
  ## geom_point(aes(X1, y), data, alpha = .5) +
  labs(y = "Marginal Prediction") ## +
  ## geom_vline(aes(xintercept = 1), linetype = "dashed")
ggsave("mp_w.png", width = 8, height = 4)
