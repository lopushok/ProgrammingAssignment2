## The functions computes and caches the inversed matrix for the given one
## If it find already calculated matrix (and the incomming matrix did not change since than)
## it returns such matrix from the cache.
## If it's not found, the function calculates inverse matrix itself.

## The function returns a list of functions 
## to set and get the values of the matrix and its inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setsl <- function(solve) m <<- solve
	getsl <- function() m
	list(set = set, get = get, setsl = setsl, getsl = getsl)
}


## Return an inverse matrix for 'x'. 

cacheSolve <- function(x, ...) {
	m <- x$getsl()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
		}
	data <- x$get()
	m <- solve(data, ...)
	x$setsl(m)
	m 
}


