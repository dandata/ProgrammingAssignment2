## makeCacheMatrix creates a list of function to set matrix, get matrix,
## set it's inversed and get it. It also possible to pass matrix as an
## argument.
##   m = matrix(c(1, 2, 3, 4), 2, 2)
##   nice_matrix = makeCacheMatrix()
##   nice_matrix$set(m)
##   nice_matrix$setinversed(solve(m))
##   nice_matrix$get() # returns orignal matrix
##   nice_matrix$getinversed() # returns inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  
  get <- function() x
  set <- function(new_matrix) {
    x <<- new_matrix
    inversed <<- NULL
  }
  
  getinversed <- function() inversed
  setinversed <- function(y) {
    inversed <<- y
  }
  
  list(get = get, 
       set = set,
       getinversed = getinversed,
       setinversed = setinversed)
}


## cacheSolve takes a list created by makeCacheMatrix,
## stores and returns inverted matrix. In all following calls
## with the same matrix cached version will be used and
## message will be in console.
##   m = matrix(c(1, 2, 3, 4), 2, 2)
##   nice_matrix = makeCacheMatrix()
##   cacheSolve(nice_matrix)
##   nice_matrix$getinvesed() # returns inversed matrix

cacheSolve <- function(x, ...) {
  inversed <- x$getinversed()
  if(!is.null(inversed)) {
    message("getting pre-calculated inversed")
    return(inversed)
  }
  
  inversed <- solve(x$get(), ...)
  x$setinversed(inversed)
  inversed
}
