## This file contains two functions that allow to construct a special
## "matrix" object that contains matrix data and its inverse, and also
## a convenience function that calculates the inverse of such matrix data
## or returned the cached inverse if already calculated.
##
## `makeCacheMatrix` should be used to create a special "matrix" object to store
## the matrix data and its inverse.
## This "matrix" should be used by `cacheSolve` to calculate the inverse of the
## matrix data - and storing it in the "matrix" - or return the cached inverse
## if already computed by an earlier call.

## 'makeCacheMatrix' creates a special "matrix" which is a list of four functions:
## - `set`: sets the matrix data of the "matrix"
## - `get`: gets (returns) the matrix data of the "matrix"
## - `getsolve`: gets (returns) the cached inverse of the matrix data inside the
##               "matrix" object. This function will return NULL if the inverse
##               has not been set through `setsolve`
## - `setsolve`: sets the matrix to be considered as the inverse of the matrix
##               returned by `get`. This function does NOT test if the matrix
##               set is actually the matrix data inverse

makeCacheMatrix <- function(mtx = matrix()) {
        slv <- NULL
        set <- function(smtx) {
                mtx <<- smtx
                slv <<- NULL
        }
        get <- function() mtx
        setsolve <- function(solved) slv <<- solved
        getsolve <- function() slv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## 'cacheSolve' calculates the inverse of the special "matrix"
## created with the `makeMatrix` function. 
## If the "inverse "matrix" inverse has already been calulated, the
## cached inverse is returned and the computation skipped.
## Otherwise, it calculates the inverse of the "matrix" and its inverse
## is cached via the `setsolve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        slv <- x$getsolve()
        if (is.null(slv)) {
                ## If not calculated before, compute the inverse and cache it
                m <- x$get()
                slv <- solve(m, ...)
                x$setsolve(slv)
        }
        slv
}
