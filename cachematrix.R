

## This function creates a matrix that can cache its inverse.
##  Requires a square invertible matrix as input(limit of solve fxn).

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    ## this sets the values of x and inv_mat in the global environment
       x <<- y
       inv_mat <<- NULL
     }
   get <- function() x
   ## this allows the program to get the matrix
   setinv <- function(inverse) inv_mat <<- inverse
   ## this sets the inverse of the matrix
   getinv <- function() inv_mat
   ## this gets the inverse of the matrix
   list(set = set, get = get,
          setinv = setinv,
           getinv = getinv)
  }

## This function either calculates the inverse of the matrix from
## function makeCacheMatrix or retrieves the calculated value from 
## the cache.
## Requires a square invertible matrix as input (limit of solve fxn).

cacheSolve <- function(x, ...) {
  ##input for cacheSolve is the output of makeCacheMatrix
  
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    ## gets cached data if it is there
    message("getting cached data")
    return(inv_mat)
  }
  ## if cache is empty, calculates the inverse
  mat_data <- x$get()
  inv_mat <- solve(mat_data, ...)
  x$setinv(inv_mat)
  return(inv_mat)
}
