## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	rev <- NULL                                     ## Create empty reverse
	set <- function(y) {                            ## Set function for matrix
		x <<- y
		rev <<- NULL
	}
	get <- function() x                             ## Get function for matrix
	setrev <- function(solve) rev <<- solve         ## Set function for reversed matrix
	getrev <- function() rev                        ## Get function for reversed matrix
	list(set = set, get = get,                      ## Return of list of functions results (please, correct me if I`m wrong)
		setrev = setrev,
		getrev = getrev)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	rev <- x$getrev()                               ## Pull cached value
	if(!is.null(rev)){                              ## If reversed matrix exists - return it
		message("here is cached data")
		return(rev)
	}	
	data <- x$get()                                 ## If no reversed matrix - then create new one
	rev <- solve(data, ...)                         ## Reversing
	x$setrev(rev)                                   ## Caching
	rev                                             ## Return results
}
