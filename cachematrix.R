## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "matrix" which is really composed of a list containing functions to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates or "solves" for the inverse of a "matrix" created with the makeCacheMatrix function above. It first checks to see whether the inverse has already been previously calculated and that the matrix has not changed. If so, then it retrieves the inverse from the cache. Otherwise, it proceeds to solve the inverse of the matrix and cache this new result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it has not been previously calculated and cache
	m <- x$getInverse()
	if(!is.null(m)) {
		message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
