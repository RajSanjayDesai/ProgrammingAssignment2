## Program containing pair of functions that can cache the inverse of matrix.
## functions creates a special "Matrix" object that can cache inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set =  set, get = get. setInverse = setInverse, getInverse = getInverse)
}

## This function computes inverse of the special "Matrix" returned by makecacheMatrix as used above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
        }
}
