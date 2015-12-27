## the function makeCacheMatrix peform the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the iverse of the matrx
## get the value of the iverse of the matrx


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    X <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## the function cacheSolve peform the following:
## checks to see if the inverse matrix has already been calculated
## if so, it gets the inverse matrix from the cache and skips the computation
## otherwise, it calculates the inverse matrix of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}