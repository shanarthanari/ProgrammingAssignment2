## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of fuctions help caching inverse of matrix. It assumes the 
## matrix is "invertible"

## Example test case: 
## setwd('<path to directory where cachematrix.R resides>')
## source("cachematrix.R")
## mtx <- matrix(c(1,2,3,4,5,6,7,8,9), 3,3)
## lmtx <- makeCacheMatrix(mtx)
## cacheSolve(lmtx)
## cacheSolve(lmtx)

## Returns a special list containg setters and getters for input vector and 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## First it checks cached value for the input matrix. If available it returns it. 
## Otherwise, it calculates inverse of the matrix and sets this value in the 
## cache then returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


