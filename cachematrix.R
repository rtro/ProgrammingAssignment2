## Put comments here that give an overall description of what your
## functions do

### The cachematrix.R code computes and caches the inverse of a matrix.

## Write a short comment describing this function


### The makeCacheMatrix function first initializes the x and
### the invx variables.  Secondly, it assigns y to the parent environment x,
### and resets the invx variable to NULL.  Then it establishes the functions to compute, save
### and retrieve the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
      x <<- y
      invx <<- NULL
  }
  get <- function() x
  setinvx <- function(solve) invx <<- solve
  getinvx <- function() invx
  list(set = set, get = get, setinvx = setinvx, getinvx = getinvx)
}


## Write a short comment describing this function

### Function first checks to see if there is a cached inverse for the inputted matrix.
### If there is no data cached, then the function finds the inverse,
### sets the invx and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinvx()
  if(!is.null(invx)) {
      message("getting cached data")
      return(invx)
  }
  data <- x$get()
  invx <- solve(data,...)
  x$setinvx(invx)
  return(invx)
}
