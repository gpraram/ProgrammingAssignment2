## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # initializes an empty inverse matrix
  
  # Set the value of the matrix
  set <- function(y) {
    # assign values to objects x and i in the parent environment
    x <<- y
    i <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x # lexical scoping
  
  # Set the value of the inverse matrix
  setInverse <- function(inverse) i <<- inverse
  
  # Get the value of the inverse matrix
  getInverse <- function() i
  
  # Return a list of named sub-functions
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix() above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve() should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse() # gets the value of the inverse matrix from the cache 
  
  # If the inverse matrix is already calculated  
  if(!is.null(i)) { 
    message('getting cached data')
    return(i) # returns the inverse to parent environment
  }
  
  # Otherwise
  data <- x$get() # gets the value of the matrix
  i <- solve(data, ...) # calculates the inverse of the matrix 
  x$setInverse(i) # sets the value of the inverse matrix
  i # returns a matrix that is the inverse of 'x'
}