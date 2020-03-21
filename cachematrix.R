## Matrix Inversion Function in R
## makeCacheMatrix and cacheSolve

## This function creates a "special" matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function will compute the inverse of the matix returned by the first function
## also, it will retrieve the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$geinverse()
  if(!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
