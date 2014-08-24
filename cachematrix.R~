## Put comments here that give an overall description of what your
## functions do
## Programming Assignment #2
## Date 8/23/2014
## Student: Albert Aumentado
## This set of functions will take a matrix x and create
## an object with a set functions for caching and retrieving
## matrices the matrices involved.
##
## example:
## > A <- matrix(rnorm(1E6), nrow = 1000, ncol = 1000) # generate a matrix
## > cacheMatrix_A <- makeCacheMatrix(A) # creates the caching object cacheMatrix_A
## > cacheSolve(cacheMatrix_A)
## # first iteration:
## <will calculate and spit out solve result>
## > cacheSolve(cacheMatrix_A) # call it again
## #second iteration
## getting cached data
## <spits out solve result cached in cacheMatrix_A>
## This reduces solve time by trading computational time for memory.

## Write a short comment describing this function
## This function creates a special caching matrix object
## a list will be returned that cacheSolve uses to access
## items inside of the caching matrix object
makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL ## initialize a null matrix for the inverse
	
	## the function below will cache the matrix x
	## also sets the initial state of inverse_x to NULL
	set <- function(y) {
		x <<- y ## cache the matrix
		inverse_x <<- NULL
	}
	
	get <- function() x ## retrieves the starting matrix
	
	setinverse <- function(solve) inverse_x <<- solve ## caches result of a solve
	getinverse <- function() inverse_x ## retrieves inverse_x if exists
	
	## note each item in the list is a function
	## this is an object that cacheSolve can use to operate on the original matrix x
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function will return the inverse of a matrix x
## the matrix x is actually stored in a caching matrix object created by makeCacheMatrix
## this is not entirely robust if the matrix is not square and/or is singular
## but an error message will appear
## note I changed the the function assignment from function(x,...)
## to function(cache_x, ...) because the variable callout was confusing
cacheSolve <- function(cache_x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- cache_x$getinverse() ## try to retrieve the cached inverse
        ## check if inverse_x return non null result (i.e. success)
        if(!is.null(inverse_x)) {
        	message("getting cached data")
        	return(inverse_x)
        }
        
        ## this block will calculate inverse_x if it does not exist
        temp_x <- cache_x$get()
        inverse_x <- solve(temp_x)
        cache_x$setinverse(inverse_x)
        inverse_x
}
