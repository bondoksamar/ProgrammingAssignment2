
## Caching the Inverse of a Matrix
## The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
##1- set the value of the matrix
##2- get the value of the matrix
##3- set the value of the inverse
##4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ##here starts part 1
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##here starts part 2
  
  get <- function() x
  
  #here starts part 3
  
  setinverse <- function(inverse) m <<- inverse
  
  #here starts part 4
  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function,cacheSolve calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## here it will check if the inverse has already been acalculated and returns the value if exist,
  ## while displaying a message "getting Cached data"
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data") 
    return(m)
  }
  data <- x$get()
  
  ## here is when it will calculate the value of the inverse when not found in cache.
  
  m <- solve(data, ...)
  
  ## here it will set the value in the cache
  
  x$setinverse(m)
  m
}	
