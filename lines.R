lines_draw = function(mu, cov,  title) {  
  det = det(cov)
  
  x = y = seq(-4, 4, 0.04 )
  z <- matrix(0, length(x), length(y))
  
  for(i in 1: length(x)){
    for(j in 1: length(y)){
      point <- c(x[i], y[j])
      z[i,j] <- (1/(sqrt((2*pi)^2*det)))* exp((-1/2)*((point-mu)%*%solve(cov)%*%t(point-mu)))
    }
  }
  
  contour(x, y, z, main=title, asp=1)
  
}

lines_draw(matrix(0, 1, 2), matrix(c(1,0,0,1), nrow=2, ncol=2),  title="Признаки коррелированы")