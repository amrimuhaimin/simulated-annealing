# let say r=10 than we've got 10x10 matrix
# define the X0 for permutation with random
# simulated annealing travel salesman with uniform distribution as reward
# k is the number of cities
# ij is the matrix that represent (distance, reward between i-th city to j-th city)
# matrix is defaulted with uniform (0,1) distribution
# iter means how many iteration the program will stop
rm(list=ls())
sim.annealing <- function(k, iter){
  U_0k <- runif(k,0,1) #reward from 0 to k city
  Uij <- matrix(runif(k*k,0,1),ncol=k) #reward from i to j city
  x0 <- sample(1:k,replace=F,size=k) #initiate permutation permutation
  swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 
  vvec <- function(xy){
    v.vec <- numeric(k)
    for(i in 2:k){
      v0k <- U_0k[xy[1]]
      vij <- Uij[xy[i-1],xy[i]]
      v.vec[i] <- vij
      v.vec[1] <- v0k
    }
    return(v.vec)
  }
  cstop = iter
  xn <- x0
  n <- 0
  vectorreward <- numeric(0)
  matroute <- matrix(ncol=k,nrow=cstop)
  repeat{
    n <- n+1
    vx <- sum(vvec(xn)) #total reward with current route
    repeat{
      ij <- sample(1:k,size=2,replace=F) #caclculate new route
      if(runif(1) < 1/choose(k,2)){
        break
      }
    }
    I <- which(xn==ij[1])
    J <- which(xn==ij[2])
    yn <- swtch(xn,I,J)
    vy <- sum(vvec(yn))
    a <- min((1+n)^(vy)/(1+n)^(vx),1)
    b <- 1-a
    if(runif(1) < a){
      xn <- yn
    } else if(runif(1) > b){
      xn <- yn
    }
    finalreward <- sum(vvec(xn))
    vectorreward[n] <- finalreward
    matroute[n,] <- xn
    if(n == cstop){
      break
    }
  }
  return(list(vectorreward,matroute))
}

