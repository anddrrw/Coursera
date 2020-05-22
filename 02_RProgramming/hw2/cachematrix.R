## This set of functions will calculate and cache the inverse of an input
## matrix, so that R only needs to perform this costly calculation once.

## Completed as part of the R Programming course on Coursera.

## makeCacheMatrix creates the initial cache of values and enables basic
## functions like get and set. Input is an invertible matrix x.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    ## Output a list containing the four functions defined above. This will
    ## allow for easy handling of cached values.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will solve the inverse of the cached matrix created with
## makeCacheMatrix(). If the result of the calculation is already stored in 
## the cache, it will simply get that value and not recalculate it.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("...Getting cached data...")
        return(i)
    }
    
    ## will only reach here if no cached inverse is found.
    mat <- x$get()
    i <- solve(mat, ...) ## assume the matrix is invertible...
    x$setinv(i)
    i
}
