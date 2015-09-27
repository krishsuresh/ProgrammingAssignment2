## Coursera R Programming
## Programming Assignment 2: Lexical Scoping

## Assignment: Caching the Inverse of a Matrix
## Create a cache matrix object to calculate and cache
## the inverse of a matrix and retrieve as required.

## Usage:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)
##
## cacheMatrix$set(M) # Change the matrix being cached.
## M <- cacheMatrix$get() # Returns the matrix being cached.
##
## cacheMatrix$setInvr(solve(data, ...)) # Private function containing cached inverse of x
## cacheMatrix$getInvr() # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
  cacheInv <- NULL
  set <- function(y) {
    x <<- y
    cacheInv <<- NULL
  }
  get <- function() x
  setInvr <- function(inverse) cacheInv <<- inverse
  getInvr <- function() cacheInv
  list(set = set, get = get,
       setInvr = setInvr,
       getInvr = getInvr)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInvr()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInvr(invFunc)
  invFunc
}