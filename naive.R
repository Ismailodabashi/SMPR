naive = function(x, Py, mu, sigm, m, n) {
  amo <- matrix(c('setosa','versicolor', 'virginica', 0, 0, 0), nrow = 3, ncol = 2)
  scores = rep(0, m)
  for (i in 1:m) {
    scores[i] = Py[i]
    for (j in 1:n){
      N=1/sqrt(2*pi)/sigm[i,j]*exp(-1/2*(x[j]-mu[i,j])^2/sigm[i,j]^2)
      scores[i] = scores[i] * N
    }
    amo[i,2]=scores[i]
  }
  class <- amo[,1][which.max(amo[,2])]
}


xl <- iris[, 3:5]
n=2 #количество признаков
m=3 #количество классов
classes <- levels(xl[,3])
Py<-table(xl[,3])/dim(xl)[1]#априорная вероятность появления классов

#Вычисление матриц мат.ожидания и отклонений
mu = matrix(0, nrow=m, ncol=n)
sigm = matrix(0, nrow=m, ncol=n)
for(i in 1:m){
  for(j in 1:n){
    temp=xl[xl[,3]==classes[i],][,j] #j-й столбец содержащий только признаки i-го класса
    mu[i,j]<-mean(temp)
    sigm[i,j]<-sqrt(var(temp))
  }
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, xlab = "Длина лепестка", ylab = "Ширина лепестка", main = "Наивный байесовский классификатор")

a=0
b=0
while(a<7){
  while(b<7){
    z <- c(b, a)
    class <- naive(z, Py, mu, sigm, m, n)
    points(z[1], z[2], pch = 21, col = colors[class], asp = 1)
    b=b+0.1
  }
  b=0
  a=a+0.1
}