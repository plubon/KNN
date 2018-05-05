Rcpp::sourceCpp('knn.cpp')

moda <- function(m){
  return(apply(m,1,
  function(x)
  {
    encoded <- rle(sort(x))
    maxValues <- encoded$values[which(encoded$lengths==max(encoded$lengths))]
    return(maxValues[sample(length(maxValues), 1L)])
  }))
}

srednia_a <- function(m)
{
  return(apply(m,1,
    function(x)
    {
      n <- mean(x)
      d <- abs(x-n)
      minValues <- x[which(d==min(d))]
      return(minValues[sample(length(minValues),1L)])
    }))
}

mediana <- function(m)
{
  return(apply(m,1,
    function(x)
    {
     n <- quantile(x, 0.5)
     d <- abs(x-n)
     minValues <- x[which(d==min(d))]
     return(minValues[sample(length(minValues),1L)])
}))
}

minkaran <- function(m, n)
{
  return(apply(m,1,
  function(x)
  {
    range <- 1:max(x)
    best <- NaN
    bestCost <- Inf 
    for(val in range)
    {
      cost <- sum(abs(x-val)**n)
      if(cost < bestCost)
      {
        bestCost <- cost
        best <- val
      }
    }
    return(best)
  }))
}

minkara1.5  <- function(m)
{
  return(minkaran(m, 1.5))
}

minkara3.0  <- function(m)
{
  return(minkaran(m, 3))
}

srednia_wazona <- function(m)
{
  return(apply(m,1,
   function(x)
   {
     w <- length(x):1
     n <- sum((w*x)/sum(w))
     d <- abs(x-n)
     minValues <- x[which(d==min(d))]
     return(minValues[sample(length(minValues),1L)])
   }))
}

err <- function(x,y)
{
  return(mean(x!=y))
}

blad_bezwzgeldny <- function(x,y)
{
  return(mean(abs(x-y)))
}

mse <- function(x,y)
{
  return(mean((x-y)**2))
}

five_fold_knn <- function(M, k, p=2)
{
  ord <- sample(nrow(M))
  reverse_ord <- order(ord)
  M <- M[ord,]
  ranges <- ceiling(quantile(1:nrow(M), c(0.2,0.4,0.6,0.8)))
  ranges <- c(1, ranges, nrow(M)+1)
  results <- matrix(nrow=0, ncol=k)
  for(i in 2:6)
  {
    X <- M[-(ranges[i-1]:(ranges[i]-1)),2:ncol(M),drop=FALSE]
    Y <- M[-(ranges[i-1]:(ranges[i]-1)),1]
    Z <- M[ranges[i-1]:(ranges[i]-1),2:ncol(M), drop=FALSE]
    labels <- knn(X, Y, Z, k, p)
    results <- rbind(results, labels)
  }
  return(results[reverse_ord,])
}

base_knn <- function(X, Y, Z)
{
  return(class::knn(X, Z, Y, k=3, use.all = FALSE))
}

random_forest <- function(X, Y, Z)
{
  X[,1] <- factor(X[,1], ordered=TRUE)
  rf <- randomForest::randomForest(X, y=as.factor(Y))
  return(predict(rf, Z))
}

logistic_regression <- function(X, Z)
{
  X[,1] <- factor(X[,1], ordered=TRUE)
  model <- MASS::polr(response ~ ., data = X)
  return(predict(model, Z))
}

naive_bayes <- function(X, Z)
{
  X[,1] <- factor(X[,1], ordered=TRUE)
  model <- e1071::naiveBayes(response ~ ., data = X)
  return(predict(model, Z))
}

five_fold_formula <- function(func, M)
{
  ord <- sample(nrow(M))
  reverse_ord <- order(ord)
  M <- M[ord,]
  ranges <- ceiling(quantile(1:nrow(M), c(0.2,0.4,0.6,0.8)))
  ranges <- c(1, ranges, nrow(M)+1)
  results <- numeric()
  for(i in 2:6)
  {
    X <- M[-(ranges[i-1]:(ranges[i]-1)),]
    Z <- M[ranges[i-1]:(ranges[i]-1),2:ncol(M), drop=FALSE]
    results <- c(results, func(X, Z))
  }
  return(results[reverse_ord])
}

five_fold <- function(func, M)
{
  ord <- sample(nrow(M))
  reverse_ord <- order(ord)
  M <- M[ord,]
  ranges <- ceiling(quantile(1:nrow(M), c(0.2,0.4,0.6,0.8)))
  ranges <- c(1, ranges, nrow(M)+1)
  results <- numeric()
  for(i in 2:6)
  {
    X <- M[-(ranges[i-1]:(ranges[i]-1)),2:ncol(M),drop=FALSE]
    Y <- M[-(ranges[i-1]:(ranges[i]-1)),1]
    Z <- M[ranges[i-1]:(ranges[i]-1),2:ncol(M), drop=FALSE]
    results <- c(results, func(X, Y, Z))
  }
  return(results[reverse_ord])
}