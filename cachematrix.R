## Put comments here that give an overall description of what your
## functions do

## special matrix that can cache it's inverse, it has 4 functions and an initialization
## initialization:   
##      x <- makeCacheMatrix( matrix )
## funtions:         
##      x$get()        gets the matrix made during initialization or set
##      x$set()        sets the matrix
##      x$getinv()     gets the cached inverse
##      x$setinv()     sets the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y             ## cache matrix
        inverse <<- NULL    ## initialise inverse matrix cache
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve calculates the inverse of the cached matrix and stores the resulting
## inverse in cache as well

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv     ## Return a matrix that is the inverse of 'x'
}
