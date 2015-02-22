#Josh Roberti
#22 Feb 2015

# This R script creates two functions that collectively calculate and cache the inverse of a matrix.  **For this assignment, assume that the matrix supplied is always invertible.***

# function #1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  This function is really a list comprising subfunctions that i) set the values of the matrix, ii) get the values of the matrix, iii) calculate the inverse of the matrix, and iv) get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#function #2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
