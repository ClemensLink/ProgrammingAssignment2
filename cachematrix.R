## Definition of two functions that cache the inverse of a matrix
## This is part of an assignment in the course "R Programming"
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD on Coursera
## see https://class.coursera.org/rprog-004

## The first function is named makeCacheMatrix
## and defines a matrix which is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialization of a variable m as the inverse property
	m <- NULL
	
	## Definition of a method to set the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Definition of a method to get the matrix
	get <- function() {
		x
	}
	
	## Definition of a method to set the inverse of the matrix
	setInverse <- function(inverse) {
		m <<- inverse
	}
	
	## Definition of a method to get the inverse of the matrix
	getInverse <- function() {
		m
	}

	## Definition of a list of the two methods	
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## The second function is named cacheSolve
## and computes the inverse of the matrix in the following way:
## If the inverse has already been calculated and the matrix hasn't changes in between,
## ... it returns the inverse from the cache. 
## If this is not the case, 
## ... it calculates the inverse using the solve() function

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()

    ## If the inverse has already been calculated and the matrix hasn't changes in between,
	## ... it returns the inverse from the cache. 
    if( !is.null(m) ) {
		message("from cache")
		return(m)
    }

    ## Getting the matrix
    data <- x$get()

	## If the inverse hasn't already been calculated or the matrix has changed in between, 
	## ... it calculates the inverse using the solve() function
    m <- solve(data, ...) %*% data

    ## Setting the inverse
    x$setInverse(m)

    ## Returning the matrix
    m
}
