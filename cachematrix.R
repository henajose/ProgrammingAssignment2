##  Caching the Inverse of a given Matrix using 'makeCacheMatrix' and 'cacheSolve' functions.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##  cacheSolve: it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

a <- matrix(data = c(4,2,3,1), nrow = 2, ncol = 2) # sample data
a
newa <- makeCacheMatrix(a)
cacheSolve(newa)

