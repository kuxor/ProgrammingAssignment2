## makeCacheMatrix and cacheSolve functions avoids unnecessary computations
## for finding inverse of a matrix if it is already calculated.


## makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        set.inverse <- function(inv) {
                inverse <<- inv
        }
        get.inverse <- function() {
                inverse
        }
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## cacheSolve function computes and caches inverse of the special "matrix"
## object and returns the cached inverse in subsequent calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse = x$get.inverse()
        if (!is.null(inverse)) {
             message("getting cached data")
             return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$set.inverse(inverse)
        inverse
}
