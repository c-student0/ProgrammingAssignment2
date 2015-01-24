## Provides cached matrix inversion.  
# Usage: 
# mymx <- rbind(c(1, -1/4), c(-1/4, 1)) # this is a reversible matrix
# myv <- makeCacheMatrix(mymx)
# cacheSolve(myv)

## makeCacheMatrix: This function creates a special "vector" object that can
## cache its inverse.
## makeCacheMatrix expects a matrix

makeCacheMatrix <- function(x = matrix()) {
    # Setup variables within the environment
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

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # solve the matrix inversion, populate the cache and return the inverse.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
