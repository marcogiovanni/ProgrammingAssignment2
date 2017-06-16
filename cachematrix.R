## inverting the matrix in a intelligent way. 

## creating a special matrix. Actually, a list. 


## this function takes as argument a matrix and as output it returns a list 
## which is then used to calculate the inverse of the matrix in an efficient way, so that it is calculate only once (the first time).


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  set<- function (y) { 
    x<<-y
    inverse <<- NULL
  }
  get<- function() x
  setinverse <- function(inv) inverse<<-inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The aim of this function is to calculate the inverse of a matrix.
## In order to be efficient, it first checks if the calculation of the same matrix has been already done previously. 
## if it was already done, then it gets it from a list, otherwise it actually calculate the inverse.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
  }
  
  
