

# function sets and gets values and inverse
library(MASS) #getting inverse to use solve function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {# to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #to get the matrix
  setinverse <- function(inverse) m <<-inverse #to set the inverse value of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...){
  m <- x$getinverse() # getting the inverse to cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}