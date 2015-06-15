## The functions in this file cache the inverse of a matrix if it
## is calculated and otherwise it may be calculated and cached.

## A function for setting and getting the value of a matrix as well
## as setting and getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setinv <- function(inv) i <<- inv
	getinv <- function() i	

	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## If the inverse of a matrix is cached then it fetches the cached
## data, otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {

	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}

