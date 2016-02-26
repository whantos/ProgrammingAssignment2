## This Functions stores the result of a inverted matrix to awoid multiple 
## calculations

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        original_matix <- x
        
        set <- function(y) {
                original_matix <<- y
                inverse_matrix <<- NULL
        }
        get <- function() {
                original_matix
        }
        setInverse <- function(i) {
                inverse_matrix <<- i
        }
        getInverse <- function() {
                inverse_matrix
        }
        invisible(list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
        }
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
        inv
}


