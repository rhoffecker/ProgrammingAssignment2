## The assignment was to create functions used for creating an inverse 
## of a sqare matrix. There are two primary functions in this 
## assibnment: makeCacheMatrix and cacheSolve.

## The makeCacheMatrix function creates the functions
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

# Solves for the inverse of 'x' and makes available  as 'm' 
	setSolve <- function(solve) m <<- solve

# Enables the solved matrix 'm' to be called
	getSolve <- function() m

# Makes available a list of the available functions
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}

## The cacheSolve function determines if the inverse of 'x' has been 
## created, is so it returns the matrix from cache; if not the inverse
## of 'x' is solved and returned.

cacheSolve <- function(x, ...) {
## Uses the getSolve function to populate 'm'
	m <- x$getSolve()
	
## If matrix 'm' is in cache, returns matrix 'm'	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
## Since matrix 'm' was not in cache, gets matrix 'x' solves for its inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	
## Returns matrix 'm'
	m
}
