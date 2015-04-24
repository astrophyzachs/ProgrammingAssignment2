## The following two functions create a special object 
## that stores a matrix and caches its inverse.

## This function creates the special matrix object
## that allows you to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- mean
  getinv <- function() inv
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This second function allows returns the inverse of
## the matrix x. If the inverse has already been computed, it 
## retrieves it and returns it. If not, it computes the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
