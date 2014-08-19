## Cache Matrix Inverse
###############################################################################

## This pair of functions stores the inverse of a given matrix, so that it can
## be used multiple times, while only being calculated once.
##
## For an invertible matrix A, use
## 		x = makeCacheMatrix(A)
## to set the matrix of interest to be A.
## Then
##		cacheSolve(x)
## returns the inverse of A, while saving this inverse in the cache.
## Future calls to cacheSolve(x) return the cached value without recalculating.
## The cache is cleared when a new matrix is loaded into makeCacheMatrix.


## This function returns a list of four functions:
##
## [1] setmatrix sets the matrix of interest and clears the cache.
## [2] getmatrix returns the current matrix of interest
## [3] setinverse stores a matrix in the cache
##		(which should be the inverse of the matrix of interest).
## [4] getinverse returns the matrix currently stored in the cache.

makeCacheMatrix <- function(x = matrix())
{
	I <- NULL
	setmatrix <- function(A)
	{
		x <<- A
		I <<- NULL
	}
	getmatrix <- function() {x}
	setinverse <- function(B) {I <<- B}
	getinverse <- function() {I}
	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matrix of interest.
## Input is a list of 4 functions produced by the makeCacheMatrix function.
##
## If there is a matrix stored in the cache, this is used for the output.
## If there is nothing in the cache, the inverse of the matrix of interest is calculated
## and stored in the cache, before being returned.

cacheSolve <- function(x, ...)
{
	I <- x$getinverse()
	if(!is.null(I))
	{
		message("getting cached data")
		return(I)
    }
	M <- x$getmatrix()
	I <- solve(M, ...)
	x$setinverse(I)
	I
}