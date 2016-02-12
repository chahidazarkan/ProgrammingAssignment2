## Because matrix inversion is a costly computation, it is
## useful to cache an inverse matrix rather than computing
## repeatedly. The functions in this file allow for the
## the compution and caching of an inverse matrix in R.


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) i <<- solve
        getinverse <- function () i
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- i$getinverse()
        if (!is.null(i)) {
                        message("getting cached data")
                        return (i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
