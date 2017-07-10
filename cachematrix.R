## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix = NULL
  set = function(y) {
    x <<- y
    invmatrix = Null
  }
  get = function(){x}
  setinv = function(inv){invmatrix <<- inv}
  getinv = function()invmatrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if((!is.null(inv))){
    message("retrieved inverse matrix")
    return(inv)
  }
  else {
    inv = solve(x$get())
    x$setinv(inv)
    x$getinv()
  }
}
