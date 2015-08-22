## makeCacheMatrix function creates an object of "matrix" to store an inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverted_m <- NULL
  
  ## sets matrix value
  set <- function(y) {
    x <<- y
    inverted_m <<- NULL
  }
  
  ## gets matrix
  get <- function() x
  
  ## stores inversed matrix 
  setinversion <- function(inversion) inverted_m <<- inversion
  
  ## returns inversed matrix
  getinversion <- function() inverted_m
  
  ## makes a list of functions as a value storage for "cacheSolve"
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## cacheSolve function creates an inverse of the matrix passed by makeCacheMatrix function. 
## If inverted_m contains cached inverted matrix cacheSolve loads the stored inverted matrix 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted_m <- x$getinversion()
  
  ## Check if the inverse has already been cached
  if(!is.null(inverted_m)) {
  
    ## If yes, print a message and the inverse  
    message("getting cached data")
    return(inverted_m)
  }
 
   ## If nothing cached, compute an inverse
  data <- x$get()
  inverted_m <- solve(data, ...)
  
  ## and put it in "inverted_m" data object via "setinversion".
  x$setinversion(inverted_m)
  
  ## then print resuls
  inverted_m
}
