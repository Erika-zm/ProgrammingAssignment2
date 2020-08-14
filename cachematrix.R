## ## Caching the inverse of a Matrix


## The function "makeCacheMatrix" creates a special matrix object that can obtain its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### The evaluation of the function "makeCacheMatrix"

my_matrix <- makeCacheMatrix()
x <- matrix(c(1:4), nrow = 2, ncol = 2)
my_matrix$set(x)
my_matrix$get()
my_matrix$setinverse(solve(x))
my_matrix$getinverse()


## The function "cacheSolve" computes the inverse of the special "matrix" returned by the first function makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

### The evaluation of the function "cacheSolve"

cacheSolve(my_matrix)