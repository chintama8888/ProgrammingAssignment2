## Catching the inverse of a matrix
##  Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly
## Below are a pair functions that are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- Null
         set <- function(y) {
                 x <<- y
                 inv <<- Null
         }
}
get <- function () x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse
     getInverse = getInverse)
}
## This function computes the inverse of the special matrix created by
## makeCacheMatrix above. If the inverse has already been calculated
## ( matrix not changed) then it should retreive the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                messsage("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv

}
