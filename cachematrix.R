##The following function generates a list that is essentially a matrix containing functions that
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##This function calaculates the inverse of the "makeCacheMatrix" function, however, it first check to if the inverse of the matrix has already
##been calculated. By doing this, it could grab the inverse from the cache and skip the computation

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
