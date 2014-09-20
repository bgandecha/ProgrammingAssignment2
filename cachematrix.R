## This entire function will help in creating a cache for the inverse of a given matrix.

## This specific function will create a set of methods used to set, get, set inverse, and get
## inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This specific function will calculate the inverse of the matrix but only after checking the cache
## for saved inverse values.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}