## Programming Assignment 2: Lexical Scoping.  Overall description:
## Matrix inversion is usually a costly computation and there may be a benefit
## to caching the inverse of a matrix rather than compute it repeatedly. This
## file contains a pair of functions that invert a matrix and cache the result.
## If the user inputs the same matrix for inversion, the cached invert is
## returned rather than being recomputed.


## makeCacheMatrix creates a list of functions used by cacheSolve to set and 
## get an input matrix and set or get the inverted matrix in the cache m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calcluates the inverse of the matrix in makeCacheMatrix.
## If the inverted matrix already exists in the cache, the stored value is 
## returned, else the inverse is computed, stored in the cache, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
