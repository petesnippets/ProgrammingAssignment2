## This file contains the solution to programming assignment 2


## returns a list containing:
## functions to get and set the matrix
## functions to get and set the inverse of the matrix
## the initial value of x is the parameter to this function
## calling setMatrix() will reset the value of x and clear the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix, 
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## returns the inverse of the supplied matrix (x)
## returns the inverse from the cache if x has not changed
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
            message("returning inverse from the cache")
            return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
