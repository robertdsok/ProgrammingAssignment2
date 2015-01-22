## Matrix inversion is usually a costly type of computation, but there may be
## benefit to caching the inverse of a matrix rather than repeatedly computing it.
## The following two functions are used to cached the inverse of a matrix

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL    
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The next function returns the inverse of the matrix. The first thing
## it is doing is checking if the inverse has already been computed.
## If so, it will retrieve the results and skips any computation.
## If not, it will compute the inverse and set the value in the cache
## through the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
