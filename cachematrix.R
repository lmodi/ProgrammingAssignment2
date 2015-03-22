## Programming Assignment 2 - Caching the Inverse of a Matrix
## The purpose of this assignement is to create a function which can cache inverse of a matrix
## R's Lexical scooping rules using operator "<<-" is used to preserve the state inside an environment
## Two functions are used to create a special matrix operation and to calculate/cache its inverse


## The first function makeCacheMatrix creates a special matrix object which can cache its inverse
## The matrix object is a list containing a function to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of inverse matrix
##    4. get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The second function cacheSolve calculates the inverse  of the matrix created with the above function
## It first checks to the see if inverse is already calculated.
## If so, it gets the inverse from the cache otherwise it computes the inverse using the solve() function and set the value of inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
