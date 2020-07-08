## This function creates a matrix that can cache the inverse of itself
## Args: x - A matrix passed in as an argument

makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse of matrix
  inv <- NULL
  
  ## setter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter for matrix
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of the matrix created
## the above function makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("obtaining and processing cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
