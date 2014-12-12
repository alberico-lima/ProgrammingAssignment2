
## makeCacheMatrix return a special matrix and functions to get its cached inverse
## cacheSolve is a driver to test the first function, calling it subsequently it will calculate 
## the inverse just in the first calling

## This function builds a special object containing a list of functions to manipulate its data(matrix)
## calling internal function "set" initialize the matrix
makeCacheMatrix <- function(x = matrix()) {
    clearinv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinv <- function() inv <<- solve(x) # funcao para inverter a matrix
    getinv <- function () inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = setinv)
}


## This function is a driver to test the first function, calling it subsequently it will calculate 
## the inverse just in the first calling, and recover the inverse from cache from second time forwards.

cacheSolve <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mtr <- x$get()
  message("calculating inverse matrix")
  x$setinv()
  inv
}
