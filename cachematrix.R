## The goal of these two functions is to create a matrix inverse caching system


## makeCacheMatrix creates a special object (a list of functions) that stores a
## matrix and the space for its inverse

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                      x <<- y
                      i <<- NULL
              }
              get <- function() x
              setinverse <- function(inverse) i <<- inverse
              getinverse <- function() i
              list(set = set, get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
}


## cacheSolve computes the inverse of that object. It checks first to see if the
## inverse has already been calculated and stored in the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        return(i)
}
