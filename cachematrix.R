## makeCacheMatrix - creates a special matrix object that can cache it's inverse
## cacheSolve - computes the inverse of the special matrix object (if inverse already calculated will retrieve inverse from cache)

## makeCacheMatrix, as with "makeVector" on Coursera example, will set value of matrix, get value of matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve, as with "cachemean" on Coursera example, computes inverse of the matrix, retrieving it from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
#testing functions
mt <- matrix(c(1,2,3,4),2,2)
print(mt)
mt1 <- makeCacheMatrix(mt)
print(mt1)
##computed solve
cacheSolve(mt1)
##cached retrival
cacheSolve(mt1)
