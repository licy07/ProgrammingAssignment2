## Put comments here that give an overall description of what your
## functions do
## 
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## The special "matrix"  is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inm <- NULL
  set <- function(y) {
    x <<- y
    inm <<- NULL
  }
  get <- function() x
  setinverse <- function(solvem) inm <<- solvem
  getinverse <- function() inm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inm <- x$getinverse()
  if(!is.null(inm)) {
    message("getting cached data")
    return(inm)
  }
  data <- x$get()
  inm <- solve(data, ...)
  x$setinverse(inm)
  inm
}
