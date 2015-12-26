## The functions in this file allows the manipulation of a special 
## matrix object that can store a matrix elements and its inverse,
## caching the inversion operation result so that we can use the 
## it as many times as we want withou recomputing it. 

## Create the special matrix that can cache its inverse.
## The makeCacheMatrix function actually builds a list containing
## functions that set or retrieve the cached matrix elements and
## its inverse. Both cached matrix and inverse is stored in the
## environment created by the makeCacheMatrix function call.
makeCacheMatrix <- function(x = matrix()) {
  
  ## x must be a matrix
  stopifnot(is.matrix(x))
  
  # Object containing the cached inverse
  inv <- NULL
  #Function that set the new matrix elements
  set <- function(y) {
    # The matrix is stored in the 'x' object of the parent environment
    x <<- y
    # The cached inverse is set to NULL
    inv <<- NULL
  } 
  #Function that retrieves the matrix elements
  get <- function() x
  #Function that sets the inverse elements
  setinverse <- function(inverse) inv <<- inverse
  #Function that retrieves the inverse elements
  getinverse <- function() inv
  #Return the list containing the set/get functions
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse )
}


## Return a matrix that is the inverse of the special matrix 'x',
## created by makeCachedMatrix. The inverse operation is performed
## only if its result is NOT cached within the matrix 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  } else {
    message("Performing inverse computation...")
    inv <- solve(x$get(),...)
    x$setinverse(inv)
    return(inv)
  }
}
