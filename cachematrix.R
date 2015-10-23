## This file has two functions, "makeCacheMatrix" and "cacheSolve".
## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {			# Create the "matrix" object. Default input is x
	m <- NULL											# Initialize the value of inversed matrix
    set <- function(y) {								# Set the input matrix
		x <<- y
        m <<- NULL
    }
    get <- function() x									# Get the input matrix
    setinversion <- function(inversion) m <<- inversion	# Set the value of the inversed matrix
    getinversion <- function() m						# Get the value of the inversed matrix
    list(set = set, get = get,							# Setup the list using the defined the functions
        setinversion = setinversion,
        getinversion = getinversion)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {						# Compute the inverse of the "matrix" returned by makeCacheMatrix.
    m <- x$getinversion()								# Get the inversion of the matrix
    if(!is.null(m)) {									# If the value is not NULL, this matrix has been computed. Just take the value from cache.
		message("getting cached data")
        return(m)
    }
    data <- x$get()										# If the value is NULL, we need to compute it. First pass the matrix to "data"
    m <- solve(data, ...)								# Secondly, invert the matrix
    x$setinversion(m)									# Set the inversion of the matrix so that we can use it later.
    m
}

