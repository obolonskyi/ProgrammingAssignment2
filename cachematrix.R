## The following code presents two functions makeCacheMatrix and cacheSolve
## The first function makeCacheMatrix caches input matrix, the second function either inverses the matrix or retrieves already inverted matrix from cache

## This function caches the value of the inverted matrix,  

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}

## Function computes invertion of the matrix if it was not computed earlier.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  
}
