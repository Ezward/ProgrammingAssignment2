#
# methods to calculate and cache the result of the inverse operation 
# on a matrix.
#
# makeCacheMatrix() is a closure that creates that cache for a matrix x.
# cacheSolve() calculates and caches the operation.
#

#
# Create a cache for a matrix inverse operation.
# This can be passed to cacheSolve() to apply the 
# inverse operation efficiently.
#
# x - the matrix to be operated upon.
# return - list of functions to manipulate the cache
#
makeCacheMatrix <- function(x = matrix()) {
  thisInverse <- NULL
  
  #
  # set the matrix
  #
  set <- function(theMatrix) {
    x <<- theMatrix
    thisInverse <<- NULL # clear the cached inverse for prior matrix
  }
  
  #
  # get the matrix
  #
  get <- function() {
    x
  }
  
  #
  # set the cached inverse
  #
  setInverse <- function(theInverse) {
    thisInverse <<- theInverse
  }
  
  #
  # get the cached inverse
  #
  getInverse <- function() {
    thisInverse
  }
  
  #
  # return as a list of functions
  # that can be called to manipulate
  # the matric value and the cached inverse
  #
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#
# Return the inverse of the matrix x created with makeCacheMatrix()
# The first time this is called, the inverse is calculated and cached.
# sSubsequent calls return the cached inverse.
# If the matrix is set to a new matrix, the cached is cleared.
#
# x - the matrix cache created by createCacheMatrix()
# ... - arguments to pass to solve(,...) along with the matrix
# return - matrix - the inverse of the matrix that was passed
#                   to createCacheMatrix()
#
cacheSolve <- function(x, ...) {
  # check the cache
  theInverse = x$getInverse()
  
  # if the cache is empty, calculate
  # the inverse and cache it
  if(is.null(theInverse)) {
    data <- x$get()
    theInverse <- solve(data, ...)
    x$setInverse(theInverse)
  }
  
  theInverse
}
