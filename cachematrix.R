# This function creates a special "matrix" object that can cache its inverse.
# The special "matrix" is really a list containing a function to
#  1. set: set the value of the matrix
#  2. get: get the value of the matrix
#  3. setInverse: set the value of the inversed matrix
#  4. getInverse: get the value of the inversed matrix
  
makeCacheMatrix <- function(x = matrix()) {

      inverse_x<- NULL 
      set <- function(y) {
	  x <<- y
	  inverse_x<<- NULL 
      }

      get <- function() x 
      setInverse <- function(inversed) inverse_x<<- inversed 
      getInverse <- function() inverse_x

      list(set = set, get = get,
	       setInverse = setInverse,
	       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.


cacheSolve <- function(x, ...) {
      m <- x$getInverse() 
      if(!is.null(m)) { 
	  message("getting cached data")
	  return(m) 
      }
      data <- x$get() 
      m <- solve(data) 
      x$setInverse(m) 
      m  # Return a matrix that is the inverse of 'x'
}

 