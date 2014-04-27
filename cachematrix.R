## The functions are used for creating, a matrix, then calculating, caching 
## and retrieving its inverse

## This function creates and caches matrix and allows accessing 
## of the matrix and its inverse

makeCacheMatrix <- function(x = numeric) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}



## this function caches the inverse of matrix so that it can be retrieved by
## the previous function

cachesolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
