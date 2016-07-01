## This script includes two functions: makeCacheMatrix() and cacheSolve().  The makeCacheMatrix() takes a matrix and makes it
## cache-able as an R list object.  The cached matrix can then be sent to the cacheSolve function which returns the
## inverse of the cached matrix.  


library(MASS) #MASS library is needed for the ginv() function which returns the inverse of a matrix

## The makeCacheMatrix() function takes a matrix and makes it cache-able as an R list object that contains functions to 
## get and set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve() function takes a cached matrix that has been produced by the makeCacheMatrix function and returns
## it's inverse.  The first time cacheSolve() is called on a particular matrix it will calculate the inverse of the 
## matrix it has been passed using the MASS library's ginv() function and cache the value within the cached matrix.  If
## the cacheSolved() function has already been called for the matrix, the cached value will be returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- ginv(data, ...)
  x$setinverse(i)
  i
}
