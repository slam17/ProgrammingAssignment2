## The functions below, first, create a special "matrix" object that can cache its inverse.
## Then it computes the inverse of said special matrix.

##The makeCacheMatrix function stores the following functions in a "matrix": sets the value of the matrix,
##gets the value of the matrix, takes the inverse of the matrix, and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  invertMatrix <- function(solve) c <<- solve
  getMatrix <- function() c
  list(set = set, get = get,
        invertMatrix = invertMatrix,
        getMatrix = getMatrix)
}


## The cacheSolve function first checks to see if the matrix in the above function was already calculated. If it has,
## it gets the inverted matrix from the cache. If it hasn't, it calculates the inverted matrix. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  c <- x$getMatrix()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$invertMatrix(c)
  c
}
