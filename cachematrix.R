## Caching the inverse.matrix of a Matrix:
## Matrix inverse.matrix is usually a costly computation and there may be some
## benefit to caching the inverse.matrix of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.matrix.

## This function creates a special "matrix" object that can cache its inverse.matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setinverse.matrix <- function(inverse.matrix) inverse.matrix <<- inverse.matrix
  getinverse.matrix <- function() inverse.matrix
  list(set = set,
       get = get,
       setinverse.matrix = setinverse.matrix,
       getinverse.matrix = getinverse.matrix)
}


## This function computes the inverse.matrix of the special "matrix" created by
## makeCacheMatrix above. If the inverse.matrix has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse.matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse.matrix of 'x'
  inverse.matrix <- x$getinverse.matrix()
  if (!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  mat <- x$get()
  inverse.matrix <- solve(mat, ...)
  x$setinverse.matrix(inverse.matrix)
  inverse.matrix
}
