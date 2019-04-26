## the assignment is to write a pair of functions 
## that cache the inverse of a matrix.


## create a special "matrix" object that can cache matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv_matrix) inv <<- inv_matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)   ## Return a matrix that is the inverse of 'x' from the cache
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv             ## Return a matrix that is the inverse of 'x'
}

