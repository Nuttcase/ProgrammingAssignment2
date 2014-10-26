## My attempt at assignment 2. Here goes nothing...

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL                ## Creates variable in local environment
     set <- function(y) {
          x <<- y             ## Supperassigns value of y to x, 
                              ## where x is makeCacheMatrix()'s input
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse ## This sets i to inverse in 
                                                   ## the parent env (makeCacheMatrix())
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
