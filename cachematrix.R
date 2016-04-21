## The two functions reduce the cost of calculating the inverse of a matrix
## by storing the calculated result within the "matrix" object itself.

## makeCacheMatrix initialises the "matrix object that has the ability of
## storing the inverse within itself as a list of functions returning the
## required variable.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of a matrix and stores the value 
## within the "matrix" object itself unless the inverse has already been
## calculated in which case the already calculated value will be returned

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                ## The function prints a message if the stored value is
                ## being returned.
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}