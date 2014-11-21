## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## trial comment

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  
  # If the matrix inverse has been calculated, return its cached value
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Calculate, using solve(), the matrix inverse, if the inverse has not yet been calculated.
  data <- x$get()
  inv <- solve(data, ...)

  # Now cache the newly created inverse
  x$setinv(inv)

  # Now return the newly cached value
  inv
}

