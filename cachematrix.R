## These functions find an inverse of a matrix and cache the inverse so R
## doesn't have to spend computing power to find the inverse if it was 
## already figured out

## This function makes a list of all the information we know about the matrix.
## Set puts in a new matrix, get displays the current matrix, getinverse shows
## the solved inverse matrix once it is solved.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix stored in the
## makeCacheMatrix function.  If the inverse was already calculated, it returns
## that value to limit computational memory.  Note: the function is not set
## up for matrix's without inverses.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    inv
  } 
    data <-x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
