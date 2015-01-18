## R Programming - programming assignment 2
## Creates and reuses a cached version of a matrix inverse to save compute time

makeCacheMatrix <- function(x = matrix()) {

	## This function creates a special "matrix" object that can cache its inverse.

  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(solve) z <<- solve
  getinv <- function() z
  list(get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {

	## This function computes the inverse of the special "matrix" created 
	## by makeCacheMatrix. If the inverse has already been calculated, 
	## then the inverse is retrieved from the cache.
	
	## Note: if the source matrix has changed then makeCacheMatrix
	## must be called first.
	
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
