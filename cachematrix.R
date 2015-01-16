## Matrix inversion is costly in terms of compute time. Caching the inverse can be 
## more efficient than re-computing it.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(solve) z <<- solve
  getinv <- function() z
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" created 
## by makeCacheMatrix. If the inverse has already been calculated, 
## then the inverse is retrieved from the cache. 

cacheSolve <- function(x, ...) {
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setinv(z)
  z
}
