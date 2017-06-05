## Matrix inversion is usually a costly computation. The two funcions below 
## address this problem by creating an object that can encapsulate the matrix
## together with a cache for its inverted matrix, so the inverted matrix will
## be computed only once (if the matrix itself didn´t change).

## This function creates a special "matrix" object that can cache its inverse.
## It assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse_matrix) im <<- inverse_matrix
  getsolve <- function() im
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. It first try to retrieve the inverse from the cache.
##  If the inverse matrix is not cached or it the matrix changed, it recomputes
##  the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getsolve()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
}
