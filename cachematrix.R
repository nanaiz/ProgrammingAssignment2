

##MakeCacheMatrix function creates a matrix, computes the inverse of the "matrix" and stores its value.




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


## CacheSolve function computes the inverse of the "matrix" created by 
## makeCacheMatrix function. In case the inverse is not NULL (and the 
## matrix has not changed), it reads the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
