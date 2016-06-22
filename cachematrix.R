## These commands allow for faster computation of matrix inverses by checking 
## for a cached inverse.  
## makeCacheMatrix() creates an object with a cached matrix inverse.  
## cacheSolve() checks its arugment for the presence of a cached inverse.

## The makeCacheMatrix() command creates a list with four function elements:
## set() replaces the matrix passed as an argument to makeCacheMatrix() to another object
## get() returns the matrix passed as an argument to makeCacheMatrix() or changed with set()
## setInv() sets a user-specified matrix inverse
## getInv() retrieves the user-specified matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse= matrix()) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve() checks its argument for a cached inverse.  
## If not null it returns the cached inverse and does not recompute it
## If null it wraps around solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
