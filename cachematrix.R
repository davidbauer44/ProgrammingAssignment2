## Programming assignment 2
## Caching matrix inversion
## https://github.com/davidbauer44/ProgrammingAssignment2


## Wrapper around a matrix that caches the inverse of the matrix
## Uses get/set, per Java standard
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solve/invert a CacheMatrix object, using the existing cached copy if available
## Ignoring all error handling
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
  if (is.null(inv)) {
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
  }
	inv
}
