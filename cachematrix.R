## inverting the matrix in a intelligent way. 

## creating a special matrix. Actually, a list.

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



## function calculating the inverse if not done yet.

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
  
  
