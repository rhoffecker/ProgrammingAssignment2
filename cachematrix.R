## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function identifies the functions
## that are used to solve for the inverse of a matrix. The output
## is a list of these functions.

makeCacheMatrix <- function(x = matrix()) {

# Sets the inverse matrix 'm' to NULL
	m <- NULL 	
		
# Takes the values for the matrix 'x' to invert
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

# Enables the original matrix 'x' to be called
	get <- function() x

# Solves for the inverse of 'x' and returns as 'm' 
	setSolve <- function(solve) m <<- solve

# Enables the solved matrix 'm' to be called
	getSolve <- function() m

# Returns a list of the available functions
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}


## The cacheSolve function determines if the inverse of 'x' has been 
## created, is so it returns the matrix from cache; if not the inverse
## of 'x' is solved and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getSolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	m

}
