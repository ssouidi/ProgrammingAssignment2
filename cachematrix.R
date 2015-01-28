## the goal of this function is to reduce CPU and time or recaculation
## as same object if it stored in the cache.
## this function will retrive an inverse of a Matix.
## it will check if it's on cache, it it's it will retun it, if not it will
## inverte the matrix and retun it.


## the function MakeCacheMatix, will set and defin the matrix.
## it will initialze all its objects to NULL.

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function (y) {
    x <<- y
    I <<- NULL
    
  }
  #get the initinal matrix
  get <- function () x
  #set the solve function to evaluate the solve function
  setInverse <- function (solve) I <<- solve
  getInverse <- function () I
  #display the result is list
  list (set = set, get = get, setInverse = setInverse,
        getInverse  = getInverse)
  
  
}



## this cacheSolve function will check if a invert matrix is in cache 
## (I  not eqaul to NULL). if it is on cache it will display it. 
## if not it will run the run the solve (x) function and store the
## inverted matrix in cache with x$SetInverse (I)

cacheSolve <- function(x = matrix (), ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse ()
  if (!is.null (I)) {
    
    message ("getting cached data")
    return (I)
    
  }
  data <- x$get ()
  I <- solve (data, ...)
  x$setInverse (I)
  I
  
}
