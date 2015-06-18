# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions work together in order to take benefit of caching the inverse of a matrix.

# makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
# It creates a list with all the functions needed:
#    - set: sets the value of the matrix
#    - get: gets the value of the matrix
#    - setinverse: sets the value of the inverse of the matrix
#    - getinverse: gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initializes the variable that stores the inverted matrix as null
  inverse <- NULL
  
  # function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # function to get the value of the matrix
  get <- function() x
  
  # function to set the inverted matrix
  setinverse <- function(inv) inverse <<- inv
  
  # function to get the inverted matrix
  getinverse <- function() inverse
  
  # returns the list of available functions
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


# makeCacheMatrix calculates the inverse of a matrix by consulting it from cache or by
# applying the solve function

cacheSolve <- function(x, ...) {
  # gets the cached inverse matrix
  inv <- x$getinverse()
  
  # if the inverse matrix is not null (already calculated before)
  if(!is.null(inv)) {
    # informs the user and returns the cached value
    message("The inverse matrix value you are getting was cached.")
    return(inv)
  }
  
  # else, computes the inverse matrix
  inv <- solve(x$get())
  
  # caches it
  x$setinverse(inv)
  
  # and returns it
  inv
}
