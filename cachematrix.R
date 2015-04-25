## Put comments here that give an overall description of what your
## functions do
## the functions support computing a matrix's inverse and storing
## the data so that it does not need to be recomputed as long as you are 
## using the same matrix

## Write a short comment describing this function
## Object to store a matrix and its inverse
## Args :
##      x: the matrix to be stored
## Object functions:
##  get :
##      returns the stored matrix
##  getsolution :
##      returns the stored inverse
##  set :
##      set the stored matrix
##    Args : 
##      y : the stored matrix
##  setsolution(solution) : 
##      set the stored inverse
##    Args : 
##      y : the inverse of the stored matrix
##      
##  returns: the cachedMatrix object
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) inverse <<- solve
  getsolution <- function() inverse
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## cacheSolve
## computes or retrieves cached matrix info from a cachedMatrix object
##  Args:
##    X: the cachedmatrix object
##  returns : the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getsolution()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setsolution(inverse)
  inverse
}
