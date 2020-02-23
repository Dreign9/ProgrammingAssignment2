## These functions are used to compute the inverse of a matrix and cache the result.

## makeCacheMatrix checks if the input is a matrix. It creates a list of functions set, get,
## setinverse and getinverse. If already computed, the function houses the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
     invm <- NULL
     set <- function(y) {
          x <<- y
          invm <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) invm <<- solve
     getinverse <- function() invm
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve receives the cached matrix. It retrieves and return the inverse matrix if already 
## computed. If there is no stored value, it computes for the inverse and stores the inverse matrix 
## in the cache,

cacheSolve <- function(x, ...) {
     invm <- x$getinverse()
     if(!is.null(invm)) {
          message("getting cached data")
          return(invm)
          ## Returns the inverse matrix from the cached data
     }
     data <- x$get()
     invm <- solve(data, ...)
     x$setinverse(invm)
     invm
     ## Return a matrix that is the inverse of 'x'
}
