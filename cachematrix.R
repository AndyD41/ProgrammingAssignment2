## Cache the Inverse of a Matrix
## Create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) i <<- inverse
		getInverse <- function() i
		list(set = set,
			 get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}

## Below function calculates the inverse of the matrix object created above.
## If it's found the inverse has already been calculated, it obtains the result from the cache.
## If not already calculated, it's done so on-the-fly.
## It's assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mt <- x$get()
        i <- solve(mt, ...)
        x$setInverse(i)
        i
}

##Testing Section
#getwd()
#setwd("C:/Users/Andy/DataScience/ProgrammingAssignment2")
#source("cachematrix.R")
#test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#test_matrix$get() #display our test matrix
#test_matrix$getInverse() ##results in NULL
#cacheSolve(test_matrix) #get the inverse
#cacheSolve(test_matrix) #results displayed from the cache
