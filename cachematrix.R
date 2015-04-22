## [makeCacheMatrix function creates a matrix object that can cache its inverse, and cacheSolve computes the inverse of the 
## matrix returned by the makeCacheMatrix. If the inverse is not changed and has already been calculated, 
## cachesolve will retrieve the inverse from the cache instead of recalculating again ]

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


##cacheSolve returns a matrix that is the inverse of x using the solve formula that is inbuilt in R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
