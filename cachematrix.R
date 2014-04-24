## These functions read an invertible matrix and checks if its inverse exists in cache. 
## If inverse exist, then it retuns that value otherwise it calculates the inverse and then save in cache

## First part reads a matrix and sets its value as null. get function simply obtains the value of x. setinverse calculates inverse of matrix and getinverse gets that inverted value

makeCacheMatrix <- function(x = matrix(,nrow=2,ncol=2)) {
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


## In this function, value stored in getinverse if passed to m. If value already exists, then if condition is fulfilled and function simply returns that value. In case no value was stored in getinverse, then if condition is not met and matrix is inverted, value is stored in cache and then returned.

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
}
        ## Return a matrix that is the inverse of 'x'







