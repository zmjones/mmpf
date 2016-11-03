**M**onte-Carlo **M**arginalization of **P**rediction **F**unctions

Contains functions which create uniform or random grids and then evaluates prediction functions on them, which allows computation of any function of the marginal distribution of the prediction function. for example, the Monte-Carlo expected value of this marginal distribution gives the partial dependence.

```{r}
library(mmpf)
library(randomForest)

data(swiss)
fit <- randomForest(Fertility ~ ., data = swiss)

pd <- marginalPrediction(swiss[, -1], "Education", c(10, 25), fit)
plot(pd$points$Education, pd$prediction, type = "o", xlab = "Education",
  ylab = "Partial Dependence")
```
