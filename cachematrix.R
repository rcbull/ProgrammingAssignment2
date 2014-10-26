## Two functions to make that cache the inverse of a matrix

## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Generate the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Cache the inverse
  x$setInverse(m)
  
  ## Return the matrix
  m
}

# test functions
x <- matrix(rnorm(9), nrow = 3)          
cx <- makeCacheMatrix(x)                  
cx$get()                                  
cacheSolve(cx)                       

