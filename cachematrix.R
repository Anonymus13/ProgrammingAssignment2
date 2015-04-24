## The functions below perform the following steps:
## makeCacheMatrix() creates a list to cache the matrix and its inverse
## to use them in further operations.
## cachesolve() verifies if there is a saved value for the inverted matrix and
## return it, and where is not it computes and chaches it.

## This function first assign a null value to minv, then use a free variable 'y'
## to set a values to the matrix x. In the same way uses solve function to
## obtain the inverted matrix and stores it in "setminv". In parallel it gets 
## and stores this values into "get" and "serinv" variables.

makeCacheMatrix <- function(x = matrix()) {
                minv <- NULL
                set <- function(y) {
                  x <<- y
                  minv <<- NULL
                }
                get <- function() x
                setminv <- function(solve) minv <<- solve
                getminv <- function() minv
                list(set = set, get = get,
                     setminv = setminv,
                     getminv = getminv)
                }


## This function retrieve and check if there is an stored value for "minv"
## in the case it exists, it returns "getting cached data", however if not
## it compute it by solve() function and stored in "serminv" variable.

cacheSolve <- function(x, ...) {
                minv <- x$getminv()
                if(!is.null(minv)) {
                  message("getting cached data")
                }
                data <- x$get()
                minv <- solve(data, ...)
                x$setminv(minv)
                minv
}
