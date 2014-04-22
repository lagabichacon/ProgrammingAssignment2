## Functions develop to cache the inverse of a matrix
## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #assume the matrix is invertible
  set<-function(y) {
    x<<-y
    inv<<-NULL
}
get<-function() x          #function to get matrix x
setInverse<-function(inverse) inv<<-inverse
getInverse<-function() inv{
  inver<-ginv(x)
  inver%*%x #function to obtain inverse of the matrix
}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve compute the inverse of makeCacheMatrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  #gets cache data
       inv<-x$getInverse()
       if(!is.null(inv)){         #checking wether inverse is NULL
         message("getting cached data")
         return(inv)              #returns inverse value
       }
       data<-x$get()
       inv<-solve(data,...)        #calculates inverse value
       x$setInverse(inv)
       inv
}
