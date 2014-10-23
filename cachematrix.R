## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## This is my makeCacheMatrix function, which is producing the same "matrix" object as in the example. 
## It's a list containing: set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is my caching function, it calculates the inverse of a specific "matrix" object created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
