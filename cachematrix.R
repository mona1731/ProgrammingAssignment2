## The following two functions will cache the inverse of a matrix once 
## if already computed.  The inverse of the matrix will be returned.


## The makeCacheMatrix function creates the matrix object and produces
## a "cached" list of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
