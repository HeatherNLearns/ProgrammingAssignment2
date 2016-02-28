## Calculate the inverse of a matrix and saves it to the cache
## Next time the user attempts to calculate the matrix inverse
## the previously saved value is returned instead of repeating
## the calculation.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## First checks to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
