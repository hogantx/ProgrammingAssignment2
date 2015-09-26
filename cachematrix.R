
## These functions store and solve a matrix and its inverse.
## if the inverse is called and already solved for the matrix,
## no additional computation is required.


## This function stores a matrix and its inverse within a function.
makeCacheMatrix <- function(x = matrix()) {
	m2 <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function caches the inverse of a matrix.
## If a new matrix is used or the inverse is not yet calculated,
## 	the inverse is solved and cached
## Else the inverse is returned.

cacheSolve <- function(x, ...) {
	m2 <- x$get() 
	i <- x$getinverse()
	if(!is.null(i) & identical(m1, m2)) {
		message("getting cached data")
		return(i)
	}
	m1 <<- x$get()
	i <- solve(m1, ...)
	x$setinverse(i)
	i
}

