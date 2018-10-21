makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ## set the matrix
  ## get the matrix
  ## set the inverse
  ## get the inverse
  ## this is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # instead of <- for the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips computation. 
    message("getting cached data...")
    return(inv)
  }
  
  # else, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache from the setinv function.
  x$setinv(inv)
  
  #return the inverse matrix
  return(inv)
}