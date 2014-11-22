## cachematrix is a suite of two functions that work together to store and retrieve
## the inverse of a matrix of interest. The inverse matrix data, once calculated, is
## stored in an object placed in an environment other than the environment in which 
## the functions operate. This allows for a stable maintenance of the data, so that
## the inverse of the matrix need not be recalculated once it exists.

## The two functions are makeCacheMatrix  and cacheSolve.

## Acknowledgements:

## My understanding of the processes necessary to this suite was enhanced by
## two sources:
## 1) John Chamber's remarks on "environments" in Chapter 5 of "Software for Data 
##    Analysis, Programming with R"
## 2) Norman Matloff's comments on using the superassignment operator in section 7.8 
##    of "The Art of R Programming". The section is titled 'Writing Upstairs'!

## Comments on the first function:

## makeCacheMatrix takes an invertible matrix as it's argument and performs two 
## important tasks. It creates an object in an environment "outside" that of
## environment of the function that will hold or "cache" the results of applying
## the solve function to the matrix. makeCacheMatrix does not employ the solve
## function itself, (that's the job of the other function in the suite), but, for
## it's second task, returns a list whose elements are the original matirx and functions
## that will allow for the transport of data to and from the "outside" environment.
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


## Comments on the second function:

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

