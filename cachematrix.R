

## this function defines matrix caching operations.
## setter and getter methods are defined for matrix and for its inverse
## in addition, there are "iscached" and "makecache" methods that check and/or set
## the flag that determines whether a new matrix was set.

makeCacheMatrix <- function(x = matrix()) {

	matrix_inverse <- NULL
	use_cache <- 0
	set <- function(y) {
		x <<- y
		matrix_inverse <<- NULL
		use_cache <<- 0
	}

	get <- function() x
	setinv <- function(m_inv) matrix_inverse <<- m_inv
	getinv <- function() matrix_inverse
	iscached <- function() use_cache
	makecache <- function() use_cache <<- 1
	list(set = set, get = get, setinv = setinv, getinv = getinv, makecache = makecache, iscached = iscached)

}



## this function returns the matrix inverse of x
## if matrix inverse has been cached, and if the matrix was not reset
## (as determined by the flag iscached), the function will return the cached
## value. otherwise, matrix inverse will be calculated, cached, and returned.

cacheSolve <- function(x, ...) {

	if (x$iscached()) {
		matrix_inverse <- x$getinv()
		message("getting cached data")
		return(matrix_inverse)
	}
	else {
		data <- x$get()
		matrix_inverse <- solve(data, ...)
		x$setinv(matrix_inverse)
		x$makecache()
		matrix_inverse
	}

}
