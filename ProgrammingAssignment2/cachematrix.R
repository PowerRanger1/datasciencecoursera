## Caching Inverse of a Matrix

## Create matric cache

makeCacheMatrix <- function(x = matrix()) {

  # Name the cached inverse of matrix
  inv <- NULL
  
  # Create set and get objects for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # Create set and get objects for matrix inverse
  
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  # Print list of functions for your matrix
  list(get=get, 
       set=set, 
       getInv=getInv, 
       setInv=setInv)
  }





## Return Matrix Inverse Cache

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  
  # Set object for data and another object to inverse the data object.
  m <- x$get()
  inv <- solve(m, ...)
  
  #Cached newly inverted matrix
  x$setInv(inv)
  
  #Print inverted matrix
  return (inv)

  }
