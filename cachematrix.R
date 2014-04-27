## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix takes matrix data type as an argument and return
## the list of get and set matrix functions, as well as get and set inverted matrix functions
## Argument value is assumed to be invertible

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## Write a short comment describing this function
## cacheSolve gets cached data if calculation has been done, Otherwise it will calculate
## and store the value in the cache 'm'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
