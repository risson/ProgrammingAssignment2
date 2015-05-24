## This R code is to write a pair of functions that cache 
## the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix"
## with the above function. However, it first checks to see if the inverse
## of the matrix has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips 
## the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse of the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
