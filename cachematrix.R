#Assignment: Caching the Inverse of a Matrix

# 1. makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( mat = matrix() ) 
{
  # Initialization of inverse property
  m <- NULL
  
  # To set the matrix
  set <- function( matrix ) 
  {
    mat <<- matrix
    m <<- NULL
  }
  
  # To get the matrix
  get <- function() 
  { mat } # To return the matrix
  
  # To set the inverse of the matrix
  setInverse <- function(inverse) 
  { m <<- inverse }
  
  # To get the inverse of the matrix
  getInverse <- function() 
  { m } # To return the inverse property
  
  # To return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# 2. cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{ 
  # To return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  # To return the inverse in case if it is already set
  if( !is.null(mat) ) 
  {
    message("getting cached data")
    return(mat)
  }
  
  # To get the matrix from our object
  data <- x$get()
  
  # To calculate the inverse using matrix multiplication
  mat <- solve(data) %*% data
  
  # To set the inverse to the object
  x$setInverse(mat)
  
  # To return the matrix
  mat
}