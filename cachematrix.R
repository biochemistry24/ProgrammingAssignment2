## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse property
    inv <- NULL
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset inverse cache if matrix changes
    }
    # Method to get the matrix
    get <- function() x
    # Method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    # Method to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Try to get the cached inverse
    inv <- x$getInverse()
    # If the inverse is already calculated, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise, calculate the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    # Cache the inverse for future use
    x$setInverse(inv)
    inv
}
