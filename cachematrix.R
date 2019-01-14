## program consist a pair of function that cache the inverse of a matrix.
## functions creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<_ NULL
}
get <- function() x
setinverse <- function(solveMatrix) inv <<- solveMatrix
getInverse <- function() inv
list(set =set, get = get, setInverse = setInverse, getInverse =getInverse)
}


## This function computes the inverse of the special "Matrix" returned by makeCaacheMatrix Above.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		retuen(inv)
		}
		data <- x$get()
		inv <- solve(data)
		x$setInverse(inv)
		inv
}
