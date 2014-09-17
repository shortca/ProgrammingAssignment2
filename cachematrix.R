## These functions calculate the inverse of a matrix then cache that value
## If that same matrix is passed in again the cached inverse value is returned
## Otherwise the inverse is calculated anew

## makeCacheMatrix sets matrix value, calculates inverse and caches result

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}



## cacheSOlve checks if inverse of matrix is already cached
## if it is cached then it returns the value
## if it is not cached it calculates inverse of matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
