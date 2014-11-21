## 
## 

## Write a short comment describing this function
## 


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## 

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

