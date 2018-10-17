## This assignment is aimed to provide a pair of function which
## named as "makeCacheMatrix", and "cacheSolve" to cache the inverse solution
## of a matrix

## This function basically creates a special matrix object which can
## cache its specific inverse solution 

makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
	set <- function(y){
		x <<- y
		inv_mat <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv_mat <<-inverse
	getinv <- function() inv_mat
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function basically computes the inverse solution of the special
## matrix returned from makeCacheMatrix function. If the solution had been
## computed, then this function would retrieve the inverse solution from the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_mat <- x$getinv()
	if(!is.null(inv_mat)){
		message("getting cached data")
		return(inv_mat)
	}
	data <- x$get()
	inv_mat <- solve(data,...)
	x$setinv(inv_mat)
	inv_mat
}
