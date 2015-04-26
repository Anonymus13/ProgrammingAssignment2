## The functions below perform the following steps:
## makeCacheMatrix() creates a list to store the values given and cached outcomes
## of the cachesolve() function.
## cachesolve() verifies if there is a saved value in the makeCacheMatrix()function
## returns it in the case it exists, and computes and chaches it in tha case it 
## doesn't

## This first function assigns a null value to minv matrix, to clear it when runs.
## then the set() function only uses a free variable 'y' to set or change 
## new values to the matrix x, defined by the user, this function doesn't store 
## any value. For this get() function stores this value to use it after.
## Similarly setminv() use solve instead of "y" to set the outcome
## matrix, and getminv() retrieve the value from the second function to store it.

makeCacheMatrix <- function(x = matrix()) {
                minv <- NULL              
                set <- function(y) {
                x <<- y
                minv <<- NULL
                }
                get <- function() x
                setminv <- function(solve) 
                minv <<- solve
                getminv <- function() minv
                list(set = set, get = get,
                     setminv = setminv,
                     getminv = getminv)
                }


## This second function in one hand retrieves and checks if there is an stored value 
## for "minv" stored by the getminv() function. Thus, in the case it exists, 
## it returns it and mention "getting cached data", however if it doesn't,
## it computes it by solve() function and stored in "minv" thourght the getminv()
## function of the first function.

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
