## Pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
  
  ## Set the matrix
      set <- function(y) {
      m <<- y
      inv <<- NULL
      }
      
      ## Get the matrix
      get <- function() {
        ## Return the matrix
        print(m)
      }
      
      ## Set the inverse
      setInverse <- function(inverse) {
        inv <<- inverse
      }
      
      ## Get the inverse
      getInverse <- function() {
        ## Return the inverse
        print(inv)
      }
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  
  
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix (from object)
  mat <- x$get()
  
  ## Calculate the inverse
  inv <- solve(mat, ...)
  
  ## Set the inverse (to the object)
  x$setInverse(inv)
  
  ## Return the matrix
  print(inv)
}

