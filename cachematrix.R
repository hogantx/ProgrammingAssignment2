
## These functions store and solve a matrix and its inverse.
## if the inverse is called and already solved for the matrix,
## no additional computation is required.


## This function stores a matrix and its inverse within a function.
makeCacheMatrix <- function(x = matrix()) {
	## Clears m2 so that matrix can be compared to previous matrix
	m2 <<- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function caches the inverse of a matrix.
## If a new matrix is used or the inverse is not yet calculated,
## 	the inverse is solved and cached
## Else the inverse is returned.

cacheSolve <- function(x, ...) {
	## Retrieves stored matrix and inverse
	m2 <- x$get() 
	i <- x$getinverse()
	## Tests if inverse needs to be calculated
	## i.e., if the current stored inverse is null, 
	## or if the matrix has changed since last refresh
	if(!is.null(i) & identical(m1, m2)) {
		message("getting cached data")
		return(i)
	}
	## Stores matrix associated with inverse to check
	## next time the inverse is called
	m1 <<- x$get()
	## Solves inverse
	i <- solve(m1, ...)
	message("solving for inverse")
	x$setinverse(i)
	i
}

