# cachematrix.R
#
# A collection of functions to:
#       a) Create a customised matrix object
#       b) Invert a matrix stored within the customised object,
#          using a cached result if the function has already been
#          called on the same object
# The code is an adaptation of the 'Caching the Mean of a Vector' example
# in the Programming Assignment 2 of the R Programming course (rprog-012)
#
# Author: James Freckleton
# Date: 20 Mar 2015


## Function: makeCacheMatrix
## Description: Creates a cacheable version of a given matrix, x.
##              The resulting object is actually a list containing get/set
##              functions; the input matrix is stored in its environment
## Note: no error handling, function expects an invertible matrix
makeCacheMatrix <- function(x = matrix()) {

        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        # ensure that the 'xinv' in the parent environment is set, using <<-
        setinv <- function(inverse = matrix()) xinv <<- inverse
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
        
}


## Function: cacheSolve
## Description: Returns a matrix that is the inverse of the given matrix, x.
##              If the inverse has already been computed for the same matrix
##              in a previous function call, the cached value is returned
##              in order to minimise runtime
## Note: no error handling, function expects an invertible matrix
##       created by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                # provide user feedback to show cached result is returned
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
