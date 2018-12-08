Mu = function(points) {
  rows = dim(points)[1]
  cols = dim(points)[2]
  mu = matrix(NA, 1, cols)
  for (col in 1:cols) {
    mu[1, col] = mean(points[, col])
  }
  return(mu)
}

CovarianceMatrix = function(points, mu) {
  rows = dim(points)[1]
  cols = dim(points)[2]
  covar = matrix(0, cols, cols)
  for (i in 1:rows) {
    a <- c(points[i,1],points[i,2])
    covar = covar + (t(a - mu) %*% (a - mu)) / (rows - 1)
  }
  return(covar)
}

PlugInCoeffs = function(mu1, sigma1, mu2, sigma2) {
  # Уравнение : a*x1^2 + b*x1*x2 + c*x2^2 + d*x1 + e*x2 + f = 0
  invSigma1 = solve(sigma1)
  invSigma2 = solve(sigma2)
  f = log(abs(det(sigma1))) - log(abs(det(sigma2))) + mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*% t(mu2);
  alpha = invSigma1 - invSigma2
  a = alpha[1, 1]
  b = 2 * alpha[1, 2]
  c = alpha[2, 2]
  beta = invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)
  d = -2 * beta[1, 1]
  e = -2 * beta[2, 1]
  return(c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y" = e, "1" = f))
}
  
  drawPoints = function(xy1, xy2) {
    x = rbind(cbind(xy1, 1), cbind(xy2, 2))
    colors = c("blue", "green")
    plot(x[, 1], x[, 2], pch = 21, bg = colors[x[, 3]], asp = 1, xlab = "X", ylab = "Y", main = "Разделяющая кривая - гипербола")
  }


  sigma1 <- matrix(c(16,0,0,10),2,2)
  sigma2 <- matrix(c(20,0,0,5),2,2)
  
  class1 <- mvrnorm(n=150,c(3,0), sigma1)
  class2 <- mvrnorm(n=200,c(5,0), sigma2)
  
  data = generateData()
  class1=data$class1
  class2=data$class2
  
  drawPoints(class1, class2)

  m1 = Mu(class1)
  m2 = Mu(class2)
  c1 = CovarianceMatrix(class1, m1)
  c2 = CovarianceMatrix(class2, m2)
  

  coeffs = PlugInCoeffs(m1, c1, m2, c2)

  x = y = seq(-50, 50, len = 1000)
  z = outer(x, y, function(x, y) coeffs["x^2"] * x ^ 2 + coeffs["xy"] * x * y + coeffs["y^2"] * y ^ 2 + coeffs["x"] * x + coeffs["y"] * y + coeffs["1"])

  contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 3, col = "darkgreen", add = TRUE)

