#################################
## Working draft of Assignment #2
#################################
## This function creates a special "CacheMatrix" object that can cache its inverse.
## IMPORTANT: We assume that all "CacheMatrix" objects created will be reflect
## invertible (i.e. Non-Singular) matrices...

makeCacheMatrix <- function(x = matrix()) {
  # Creates instance of "CacheMatrix" object
  inv <- NULL  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv <<- inverse
  get.inverse <- function() inv
  
  list(set = set, get = get, 
       set.inverse = set.inverse, 
       get.inverse = get.inverse)
}


## This function implements a "cached" version of the built-in R function "solve",
## created specifically to handle "CacheMatrix" objects returned by
## "makeCacheMatrix" above.
cacheSolve <- function(x, ...) {
  # Returns the inverse of "CacheMatrix"
  #
  # Args:
  #   x: An instance of "CacheMatrix" object created by makeCacheMatrix,
  #      assumed to be invertible (which implies that x is a square matrix)
  #
  # Returns:
  #   The inverse matrix associated with x (either retrieved from cache or first
  #   calculated and THEN cached)

  inv <- x$get.inverse()
 
  if (!is.null(inv)) {
    message("getting cached data")  # Note: this message can be commented out
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inverse(inv)
  inv
}
