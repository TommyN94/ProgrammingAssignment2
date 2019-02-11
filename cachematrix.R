## The purpose of these functions is to create a matrix using
## makeCacheMatrix() and to invert it using cacheSolve(). If the
## inverse has been computed once it is cached and can be retrivied
## in the future without recalculating it. If following the inverse
## calculation the original matrix is changed, the cache will be emptyed
## and in the next call to solveMatrix() the new inverse will be
## calculated and cached.

## Creates a matrix which is able to cache its inverse following its computation
makeCacheMatrix <- function(m = matrix()) {
   inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Either retrieves an existing inverse of a matrix or if not existing computes it and caches it
## for future retrieval
cacheSolve <- function(x, ...) {
   inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinv(inverse)
  inverse
}
