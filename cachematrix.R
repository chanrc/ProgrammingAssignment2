## The objective is to create a function which will evaluate the inverse of a matrix
## using the "solve" command.
## If the matrix has already been solved, the answer will be stored in a cache object
## ready to be retrieved using the $getinv function.

## This saves computing time by storing and getting the answers from memory rather than 
## having to compute the inverse everytime the function is called.

## The makeCacheMatrix function returns a list containing 4 functions
## $set, $get, $setinv, $getinv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cachesolve function first checks if the inverse of a matrix has already been
## computed.  If it is, it will retrive the answer from memory, printing a message
## to notify the user that the answer is obtained from cache.

## If the answer has not been previously solved, m will be Null, in which case the
## function will continue on to evaluate the answer by using the solve command.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)  ## m is not the answer, it will no longer be "null"
        x$setinv(m)
        m
}
