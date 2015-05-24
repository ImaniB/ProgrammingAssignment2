## The following two functions cache and compute the inverse of a matrix   

## The following function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 	# return list of functions of type vector
}

## The following function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {				# if the inverse was cached - 
                message("Getting cached data")
                return(inv)				# exit program 
        }
        data <- x$get()					# else 
        inv <- solve(data, ...)				# compute the inverse of the matrix
        x$setinverse(inv)				# cache the inverse
        inv						# return the inverse
}