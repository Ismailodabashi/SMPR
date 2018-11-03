

euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kwNN <- function(xl, z, k, q) 
{ 
  orderedXl <- sortObjectsByDist(xl, z) 
  n <- dim(orderedXl)[2] - 1 
  v1 <- c('setosa', 'versicolor', 'virginica')
  v2 <- c(0,0,0)
  
  for(i in 1:k){ 
    orderedXl[i, 4] = q^i 
  } 
  
  a=n+1 
  b=n+2 
  classes <- orderedXl[1:k, a:b]
  
  v2[1]=sum(classes[classes$Species=='setosa', 2])
  v2[2]=sum(classes[classes$Species=='versicolor', 2])
  v2[3]=sum(classes[classes$Species=='virginica', 2])
  
  amo <- cbind(v1,v2)
  
  class <- v1[which.max(v2)]
  return (class) 
}


colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, xlab = "Длина лепестка", ylab = "Ширина лепестка", main = "Классификация объекта (2.5; 1.5) при k=3")


z <- c(2.2, 1.2)
k <- 10
q <- 0.8
xl <- iris[, 3:5]
class <- kwNN(xl, z, k, q)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)

