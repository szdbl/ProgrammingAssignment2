## Caching the Inverse of a Matrix
## Since Matrix inversion is a costly computation, the functions below are created to cache the inverse of a matrix 
## instead of repeated computation, by creating a special object that stores a matrix and cahces its inverse.

## This function creats a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # inv stands for inverse
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by the makeCacheMatrix function. 
## It first check if the inverse has been calculated. If so, it gets the inverse from chache and skips the computation.
## Otherwise it executes the inverse computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()  # mat stands for matrix
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
