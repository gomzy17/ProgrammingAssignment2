

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   #holds the inverted matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x  # returns the input matrix
  setinvmatrix <- function(solve) m <<- solve  # sets the inverted matrix # <<- operator assigns the inverted matrix in a different env
                                               # which enables caching 
  getinvmatrix <- function() m  # returns the inverted matrix (which was cached earlier)
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()  # null if uncalculated since assigned NULL in the previous function
  if(!is.null(m)){  # if the inversion result is there
    message("getting cached data!")
    return(m)  # returns the calculated inversion
  }
  matrix <- x$get()
  m <- solve(matrix, ...)  # matrix inverted using this function
  message("matrix inverted!")
  x$setinvmatrix(m)  # this function calls does the caching for retrieval in subsequent calls 
  m
}
