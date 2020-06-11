## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( matr = matrix() ) {
  
  inv <- NULL
  
  ## Let's set the matrix
  s_mtrx <- function( matrix ) {
    matr <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  g_mtrx <- function() {
    matr
  }
  
  ## Set the inverse of our matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of our matrix
  getInverse <- function() {
    inv
  }
  
  list(s_mtrx = s_mtrx, g_mtrx = g_mtrx,setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(mtrx, ...) {
  
  m <- mtrx$getInverse()
  if( !is.null(m) ) {
    return(m)
  }
  dat <- mtrx$g_mtrx()
  
  ## Making inverse
  m <- solve(dat) %*% dat
  
  ## Setting the inverse to object
  mtrx$setInverse(m)
  
  m
}