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

kNN <- function(xl, z, k)
{
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1

	classes <- orderedXl[1:k, n + 1]

	counts <- table(classes)

	class <- names(which.max(counts))
	return (class)
}

Loo <- function(k,xl)
{
	sum <- 0
	for(i in 1:dim(xl)[1]){
		if(i==1){
				tmpXL <- xl[2:dim(xl)[1],]
			}
			else if (i==dim(xl)[1]) {
				tmpXL <- xl[1:dim(xl)[1]-1,]
			}
			else {
					
				tmpXL <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],])
			}

		xi <- c(xl[i,1], xl[i,2])
		class <-kNN(tmpXL,xi,k)
		if(class != xl[i,3])
		sum <- sum+1
	}
	return(sum)
}

xl<- iris[,3:5]

minErr = 99999

k=1

for(i in 1:dim(xl)[1])
{

curErr <- Loo(i,xl)
if(curErr <minErr)
{
minErr=curErr
k=i
}
}



