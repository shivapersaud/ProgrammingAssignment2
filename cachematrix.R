## These functions return the inverse of the supplied matrix. The inverse is
## either calculated in cachesolve() and cached by makeCacheMatrix.

## makeCacheMatrix() creates a matrix-type object that has set and get methods
## for it's inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <- function(y) {
                x <<- y
                m <<- NULL
        }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ## Create a list of pointers for methods that operate on this matrix-like object.
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve() attempts to get the inverse of "x" if it was already calcualted.
## If it was not already calculated, it will calcualte it using solve() and stores it in a cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()

	if (!is.null(m)) {
	return(m)
	}

  ## Check to see if inverse was already calculated. If so, return from cache. Else compute and store it.
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
