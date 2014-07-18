## makeCacheMatrix creates a special matrix object, and then cacheSolve
##returns the inverse of the matrix obtained from makeCacheMatrix. 
## If the inverse of the matrix is already in the cache, 
##cacheSolve fetches it, otherwise matrix is processed to obtain its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
  x <<- y
  inv_x <<- NULL
  }
  get <- function() {
    x
  }
  setinv<- function(inverse) {
    inv_x <<-inverse
  }
  getinv <- function() {
    inv_x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  
  m<-x$getinv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}
