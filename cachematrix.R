## makeCacheMatrix and cacheSolve work together to calculate, store, and retrieve
## the inverse of a given matrix

## makeCacheMatrix: creates a set of four functions for interacting with a matrix x

makeCacheMatrix <- function(x = matrix()) {
		## set inverse as NULL until calculated
        inv <- NULL
        
        ## create fn to store the given matrix 
        ## and reset the inverse to NULL since the matrix has changed
        setmt <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## create fn to retrieve the stored matrix
        getmt <- function() 
        x
        
        ## create fn to store the inverse of the matrix
        setinverse <- function(z) {
        	inv <<- z
        }
        
        ## create fn to retrieve the inverse matrix stored in "inv"
        getinverse <- function() 
        inv
        
		## return the set of functions
		list(setmt = setmt, getmt = getmt,
		setinverse = setinverse,
		getinverse = getinverse)

}




## cacheSolve: Return the inverse of a matrix stored in object 'x', where 'x' is created
## using makeCacheMatrix, either by retrieving the cached value if available
## or calculating from scratch

cacheSolve <- function(x, ...) {
		## Retrieve saved inverse matrix , if it is not NULL, and end function
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        
        ## If function is still going, there was no cached inverse
        ## Pull matrix to be inverted
        data <- x$getmt()
        
        ## Calculate inverse
        inv <- solve(data, ...)
        
        ## store the inverse back in x
        x$setinverse(inv)
        
        ## display the inverse matrix
        inv
}

