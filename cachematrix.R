## This function creates an instance of a matrix object.
## This object contains it's matrix and its inverse as well 
## as well as the functions to set and retrieve the matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(matInv) inv <<- matInv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This function returns a inverse matrix for a matrix object 
## created by the makeCacheMatrix function. 
## It first checks whether the matrix object has the inverse of
## the matrix sotred to reduce duplicate computations.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  return(inv)
    
}
