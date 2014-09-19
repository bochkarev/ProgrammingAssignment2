## Functions for cached calculations of matrix inverse.

## makeCacheMatrix for given matrix creates a special list
## containing following functions:
## get -- for getting the original matrix
## set -- for setting the new matrix
## setInv -- for setting calculated value of matrix inverse
## getInv -- for getting pre-calculated matrix inverse (NULL by default)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(newInv) {
    inv <<- newInv
  }
  getInv <- function() {
    inv
  }
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve calculates matrix inverse for a given list
## that is an output from makeCacheMatrix function.
## If input matrix already has inverse value cached,
## the cached value is returned. Otherwise the value is calculated
## and cache is filled.
## All additional arguments will be passed to solve() function.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    ## Found inverse value in cache
    return(inv)
  }
  ## Need to calculate value, because cache is empty
  inv <- solve(x$get(), ...)
  x$setInv(inv)
  inv
}
