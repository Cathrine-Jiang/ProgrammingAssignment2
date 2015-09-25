## These two functions are used to find inverse of a matrix in cache.
## If can't, then calculate it and set it in cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setinverse <- function(x) iv <<- x
        getinverse <- function() iv
        list(set = set , get = get , 
             setinverse = setinverse , getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated,  
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        message("calculating")
        data <- x$get()
        iv <- solve(data)
        x$setinverse(iv)
        iv
}
