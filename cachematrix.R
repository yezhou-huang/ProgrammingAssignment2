## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we 
## will not discuss here). In this assignment, I am supoosed to write 
##  a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL
  set <- function(y) {
    x <<- y
    x.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x.inverse <<- inverse
  getinverse <- function() x.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
