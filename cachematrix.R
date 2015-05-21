## This pair of functions creates a special object which stores a matrix
## and caches its inverse.

## This function creates a special "matrix" object, which is actually
## a list containing a function to set the value
## of the matrix, ges the value of the matrix, set the value of the inverse
## matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created by the makeCacheMatrix function. First, it checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse via the setinverse function. This
## function returns a matrix which is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
