## The makeCacheMatrix assigns functions to variables that includes using the solve 
## function to cache the inverse of a matrix. The cacheSolve function computes the 
## inverse of the matrix created by makeCacheMatrix. If the inverse has already 
## been calculated (and the original matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.

## makeCacheMatrix creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setinverse <-function(solve) m<<- solve
  getinverse <-function() m
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix above 
## or (if available) retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get ()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}
