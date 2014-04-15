## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

require(MASS)

makeCacheMatrix <- function(x = matrix()) {
        # invese is the cache result of matrix x
        # x is the matrix need be inversed
	inverse <- NULL
	#set/get for x operation
	set <- function(y) {
	        x <<- y
		inverse <<- NULL
        }
	get <- function() x
	#setinverse/getinverse for inverse result operation
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# use ginv in MASS package to invert matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Try cache first
	inv <- x$getinverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	#cache failed, So call ginv calculate inverse
	inv <- ginv(x$get(), ...)
	x$setinverse(inv)
	inv
}

#test code
# x <- rnorm(100,5,10)
# m1 <- makeCacheMatrix(matrix(x,20,5))
# m2 <- makeCacheMatrix(matrix(x,10,10))

# cacheSolve(m1)
# cacheSolve(m1)
# cacheSolve(m2)
# cacheSolve(m2)
