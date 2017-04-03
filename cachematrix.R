
## Create special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    inverse = NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Return a matrix that is the inverse of 'x'. Parameter x is makeCacheMatrix object
## If inverse has been already calculated, cached value is returned
cacheSolve <- function(x, ...) {
        
    m <- x$getInverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setInverse(m)
    
    m
}
