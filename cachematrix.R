## The two functions here are used to invert a given (invertible) matrix
## and then store the inverted matrix, which can be retrieved later
## so that there is no need to re-invert the given matrix again.
## 
## Note: you need to first call makeCacheMatrix with the matrix as the argument,
## before you can use cacheSolve to evaluate and store the inverted matrix

## makeCacheMatrix: converts the matrix of interest into a list of functions 
## that will operate on that matrix and its inverse
## set_mtrx       assigns the matrix to be inverted, and 
##                clears the cached inverted matrix
## get_mtrx       retrieves the stored matrix from above
## setinverse     assigns the inverted matrix to the cache
## getinverse     retrieves the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
      mtrx_inv <- NULL
      
      set_mtrx <- function(x0 = matrix()) {
            x <<- x0
            mtrx_inv <<- NULL
      }
      get_mtrx <- function() x
      
      set_inv <- function(inv) mtrx_inv <<- inv
      get_inv <- function() mtrx_inv
      
      list(set_mtrx = set_mtrx, 
           get_mtrx = get_mtrx,
           set_inv = set_inv, 
           get_inv = get_inv)
}

## cacheSolve: calls the functions listed in makeCacheMatrix
## retrieves the inverted matrix if it is cached
## or inverts, then caches the matrix if the cache is empty

cacheSolve <- function(x, ...) {
      mtrx_inv <- x$get_inv()
      if (!is.null(mtrx_inv)){
            message("retrieving cached inverse matrix")
            return(mtrx_inv)
      }
      
      mtrx0 <- x$get_mtrx()
      mtrx_inv <- solve(mtrx0)
      x$set_inv(mtrx_inv)
      mtrx_inv
}
