makeCacheMatrix <- function(x = matrix()) {
    ## Returns a special "matrix" that remembers its inverse
    # The special "matrix" returned is a list containing a function to 
    # set the value of the matrix
    # get the value of the matrix
    # set the value of the inverse
    # get the value of the inverse
    
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    get <- function() x
    setinv <- function(i) xinv <<- i
    getinv <- function() xinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # This calculates the inverse of the special "matrix" created with the above 
    # function. However, it first checks to see if the inverse has already been
    # calculated. If so, it gets the inverse from the cache and skips the 
    # computation. Otherwise, it calculates the inverse of the matrix and sets 
    # the value of the inverse in the cache via the setinv function.
    
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}

## Test data

# m<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# m$get()
# m$getinv()
# cacheSolve(m)
# cacheSolve(m)
# m$getinv()

# m <- makeCacheMatrix(rbind(c(1,2,3), c(0,1,4), c(5,6,0)))
# m$getinv()
# cacheSolve(m)
# cacheSolve(m)
# m$getinv()