## Put comments here that give an overall description of what your
## functions do

## This function makes a special "matrix"! It's actually not a matrix.
## It's a list of four functions to: 1-set a matrix 2-get it
## 3-set its inverse 4-get it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function first look to see if the inverse of x is assigned,
## if so just returns it. If not calculates and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,  ...)
  x$setInverse(m)
  m
}