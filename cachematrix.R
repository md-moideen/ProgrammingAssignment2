##
## This program has 2 functions:
##
## a) makeCacheMatrix --> Takes an inversible matrix as input and returns a special vecor
##    with the following functions
##    1. set the value of the input matrix
##    2. get the value of the input matrix
##    3. set the value of the inverse matrix
##    4. get the value of the inverse matrix
##
## b) cacheSolve --> Calculates the inverse of the input matrix. It first checks to see
##    if an inverse matrix has already been calculated. If so, it gets the inverse matrix
##    from the cache (using "get" function) and skips the inverse calculation(solve function).
##    If the inverse is not available, then it calculates the inverse and sets the inverse
##    matrix in the cache via "setinverse" function.
##

## This function takes a matrix as input, stores it in an environment which is different
## from the current environment. It also returns a special vector with get/set functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## Return the list with all 4 get/set functions above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns calculates and returns the inverse of the matrix
## If the inverse matrix is already available in the environment, then it skips
## calculating the inverse, and returns the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## Inverse matrix not available in the cache
  ## calculate and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
