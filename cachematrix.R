## Overall description of the two following functions:
## We define a function that creates a "matrix object", which can cache the value
## of a matrix and its inverse. We then define a function that calculates the inverse
## of the matrix if it has not been calculated already.

## The function makeCacheMatrix creates a sort of "matrix object", which caches
## the values of a given matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The function cacheSolve takes as input a "matrix object" created by
## "makeCacheMatrix" above and calculates the inverse of the matrix
## if it has not already been calculated and otherwise displays the message
## "getting cached data".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i<- solve(data)
    x$setinv(i)
    i
}
