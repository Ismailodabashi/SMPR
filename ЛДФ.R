library(MASS)

LDFCoeffs = function(mu1, sigma, mu2) {
  # Уравнение : a*x1 + b*x2 + c = 0
  invSigma = solve(sigma)
  c = log(abs(det(sigma))) - log(abs(det(sigma))) + mu1 %*% invSigma %*% t(mu1) - mu2 %*% invSigma %*% t(mu2);
  beta = invSigma %*% t(mu1) - invSigma %*% t(mu2)
  a = -2 * beta[1, 1]
  b = -2 * beta[2, 1]
  return(c("x" = a, "y" = b, "1" = c))
}

# Восстановление центра нормального распределения
estimateMu = function(points) {
  rows = dim(points)[1]
  cols = dim(points)[2]
  mu = matrix(NA, 1, cols)
  for (col in 1:cols) {
    mu[1, col] = mean(points[, col])
  }
  return(mu)
}

estimateCovarianceMatrix = function(xy1, mu1, xy2, mu2) {        
  rows1 = dim(xy1)[1]
  rows2 = dim(xy2)[1]
  rows = rows1 + rows2
  cols = dim(xy1)[2]
  sigma = matrix(0, cols, cols)
  
  for (i in 1:rows1){
    a <- c(xy1[i,1],xy1[i,2])
    sigma = sigma + (t(a - mu1) %*% (a - mu1))
  }
    
  for (i in 1:rows2){
    a <- c(xy2[i,1],xy2[i,2])
    sigma = sigma + (t(a - mu2) %*% (a - mu2))
  }
  
  return(sigma / (rows + 2))
}

drawPoints = function(xy1, xy2) {
  x = rbind(cbind(xy1, 1), cbind(xy2, 2))
  colors = c("blue", "green")
  plot(x[, 1], x[, 2], pch = 21, bg = colors[x[, 3]], asp = 1, xlab = "X", ylab = "Y")
}

sigma1 <- matrix(c(2,1,1,2),2,2)
sigma2 <- matrix(c(2,1,1,2),2,2)

class1 <- mvrnorm(n=100,c(2,2), sigma1)
class2 <- mvrnorm(n=100,c(10,0), sigma2)

drawPoints(class1, class2)

center1 = estimateMu(class1)
center2 = estimateMu(class2)
covar = estimateCovarianceMatrix(class1, center1, class2, center2)

coeffs = LDFCoeffs(center1, covar, center2)

x = y = seq(-50, 50, len = 1000)
z = outer(x, y, function(x, y) coeffs["x"] * x + coeffs["y"] *y + coeffs["1"])

contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 3, col = "darkgreen", add = TRUE)


