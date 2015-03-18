# That's the result for Programming Assignment 2 on a R Programming Course
#

# function makeCacheMatrix creates a special "matrix" object that can 
# cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mrx) m <<- mrx
    getmatrix <- function() m
    
    # return a special list of functions
    list(set = set, 
         get = get, 
         setmatrix = setmatrix, 
         getmatrix = getmatrix)
}

# Function cacheSolve computes inverse of the special "matrix" 
# returned by mackCacheMatrix
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) { # check if inversed matrix was already calculated
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # calculate inverse of a matrix by using solve()
    x$setmatrix(m)        # save calculated value to cache variable m
    m
}

# To make a matrix inversed it is necessary to call makeCacheMatrix first 
# by passing matrix as an argument. Then you should call cacheSolve 
# to calculate inverse of the matrix. 
