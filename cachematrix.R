## Preliminary draft of Assignment #2

## Put comments here that give an overall description of what your
## functions do
# Remember: For this assignment we assume the matrices are invertible.

## According to README:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
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


## According to README:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatric above. (If inverse has already been calculated and the matrix
## has not changed then cacheSolve should retrieve the inverse from the cache).

cacheSolve <- function(x, ...) {
    inv <- x$get.inverse
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}
