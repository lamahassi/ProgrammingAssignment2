## The following functions allow to compute and cache the inverse of a matrix


## This function takes a matrix, sets the inverse of the matrix, 
## then gives the inverse of the matrix.

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


## This function takes a matrix and gives its inverse, 
## but first it checks if the inverse was previously computed. 
## If yes, it returns the previously found inverse, if not, 
## it computes the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
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
