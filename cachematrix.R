## This functions enables to cache the result of the solve (inverse
## matrix) operation from a invertible matrix

## This functions returns a list containing the methods for accessing
## the invertible matrix or the cached inverse matrix (get, set, 
## getinvmatrix, setinvmatrix)
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) im <<- invmatrix
  getinvmatrix <- function() im
  list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}


## Tests if the a cached result is present in x, otherwise operates
## and cache the result of solve into x. Always returns the inverse of
## matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinvmatrix()
  if(!is.null(im)){
    message("gettting cached inverse matrix")
    return (im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinvmatrix(im)
  im
}
