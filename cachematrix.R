## program consist a pair of function that cache the inverse of a matrix.
## functions creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invMatrics <- NULL
	setMatrics <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}
	
	
getMatrix <- function() x
setinverse <- function (inverse) invMatrix <<- inverse
getInverse <- function() invMatrix
list(setMatrics =setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse =getInverse)
}


## This function computes the inverse of the special "Matrix" returned by makeCaacheMatrix Above.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(invMatrix)
	}
	
		Matrixdata <- x$getMatrix()
		invMatrix <- solve(MatrixData,...)
		x$setInverse(invMatrix)
		return(Matrix)
}

