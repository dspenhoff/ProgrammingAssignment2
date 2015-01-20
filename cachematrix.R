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
  list(set=set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, y = NULL, ...) {

	## This function computes the inverse of the special "matrix" created 
	## by makeCacheMatrix. 
	
	## x is a "special" matrix object created by makeCacheMatrix
  
  ## y is an optional "regular" matrix. If provided, it will be compared
  ## with the source matrix for x and if they are different the cache
  ## matrix will be updated with y. If y is not provided, it will be assumed
  ## that the source matrix for x has not changed.
  
  ## If the matrix has not changed or inverse has already been calculated, 
  ## then the inverse is retrieved from the cache. Otherwise, the inverse
	## is calculated, cached and returned.
  
  ## update the cached matrix if necessary
  if(!is.null(y) && !identical(x$get(), y)) {
    message("updating the cached matrix")
    x$set(y)
  }

  ## use the cached inverse if it exists
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached inverse")
    return(z)
  }
  
  ## calculate, cache and return the inverse
  data <- x$get()
  z <- solve(data, ...)
  x$setinv(z)
  z
}
