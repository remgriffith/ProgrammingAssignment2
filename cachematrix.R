## The two functions in this script cache the inverse of a square invertible
## matrix in order to save time and computing resources.

## The first function creates a list of four functions which set the value
## of the matrix, get the value of the matrix, set the value of the
## inverse of the matrix, and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) i <<- inverse
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following function first checks to see if the inverse matrix has 
## already been calculated, and if it has, it pulls the inverse matrix
## from the cache. If the inverse has not already been calculated, it 
## calculates the inverse matrix using the 'solve' function and sets its
## value using the 'setmatrix' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setmatrix(i)
  i
}
