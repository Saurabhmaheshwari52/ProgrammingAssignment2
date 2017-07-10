## The function makeCacheMatrix creates a cached matrix, while the cacheSolve
## returns the cached matrix inverse

## makeCacheMatrix creates a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix = NULL
  set = function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get = function(){x}
  setinv = function(inv){invmatrix <<- inv}
  getinv = function()invmatrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve looks for the inverse, if present or computes and returns the
## value of the inverse

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
