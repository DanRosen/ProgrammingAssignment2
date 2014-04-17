# Peer assessment for R Programming Course
# This file contains two R functions:
# makeCacheMatrix() and cacheSolve()

# function that returns a list of functions
# to handle the matrix inversion. takes a matrix as an 
# argument and returns a list of four functions:
# set: assigns the matrix to the parent environment for caching
# get: returns the matrix from the current environment
# setsolve: inverts the matrrix and assigns to the parent environment
# getsolve: gets the inverse matrix from the parent environment 
# the function must be assigned to a variable, for example:
# x <- matrix(1:4,2,2)
# z <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                  # storage space
        set <- function(y) {
                x <<- y                            # put to parent env
                m <<- NULL
        }
        get <- function() x                        # return from current env
        setsolve <- function(solve) m <<- solve    # put inv in parent env
        getsolve <- function() m                   # get inv from parent env
        list(set = set, get = get,                 # list of functions
             setsolve = setsolve,
             getsolve = getsolve)
}

# function paired with makeCacheMatrix to invert a matrix in the 
# current environment or returns the cached inverse from the parent 
# enviroment. example of use: 
# cacheSolve(z)
# takes as an argument a special matrix created by makeCacheMatrix
# the first time the function is called on a special matrix the inverse
# is calculated and cached. the second time the function is called on the
# same special matrix the inverse is returned from the cache and not
# calculated again

cacheSolve <- function(x, ...) {
        m <- x$getsolve()      # see if the inverse is cached
        if(!is.null(m)) {      # if the inverse is cached
                message("getting cached data")
                return(m)      # return the cached inverse
        }
        data <- x$get()        # otherwise get the matrix
        m <- solve(data, ...)  # calculate the inverse
        x$setsolve(m)          # cache the inverse
        m                      # return the calculated inverse
}

example of use
# x <- matrix(1:4,2,2)
# z <- makeCacheMatrix(x)
# cacheSolve(z)

