## default loss functions based
defaultLoss = function(x, y) UseMethod("defaultLoss")
defaultLoss.numeric = function(x, y) mean((x - y)^2)
defaultLoss.factor = function(x, y) mean(x != y)
defaultLoss.integer = function(x, y) defaultLoss.numeric(x, y)
defaultLoss.ordered = function(x, y) defaultLoss.numeric(x, y)
defaultLoss.character = function(x, y) defaultLoss.factor(x, y)
## assumes first argument is the matrix
defaultLoss.matrix = function(x, y) apply(x, 2, function(z) defaultLoss(z, y))
