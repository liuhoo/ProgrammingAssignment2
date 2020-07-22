## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function sets and gets the original and inverse matrix.
makeCacheMatrix <- function(x = matrix){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inv) m <<- inv
    getInverse <- function() {m}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function returns the inverse of a matrix. 
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}