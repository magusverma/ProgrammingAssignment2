## Code Written by Magus Verma for R Course of Coursera , Week 2 Programming Assignment 

## This defines the special "matrix" object needed for assignment 
# Start with something like x <- makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
	# inverse_of_x holds the inverse of the matrix x computed using setsolve() below
	inverse_of_x <- NULL

	# Setting matrix for a makeCacheMatrix object
	# run using something like
	# x$set(matrix(1:4,2,2))
    set <- function(y) {
            x <<- y
            inverse_of_x <<- NULL
    }
    
    # Gettings matrix for a makeCacheMatrix object
	# run using something like
	# x$get()
    get <- function() x

    # setsolve,getsolve are used setter and getter for inverse of x 
    setsolve <- function(solve) inverse_of_x <<- solve
    getsolve <- function() inverse_of_x
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
# To verify run something like following twice , if you have an object like x of makeCacheMatrix already up
# cacheSolve(x) 
cacheSolve <- function(x, ...) {
    
    inverse_of_x <- x$getsolve()
    if(!is.null(inverse_of_x)) {
            message("getting cached data")
            return(inverse_of_x)
    }
    data <- x$get()
    inverse_of_x <- solve(data, ...)
    x$setsolve(inverse_of_x)
    inverse_of_x
}

# inverse can be checked using something like
# cacheSolve(x) %*% x$get()
