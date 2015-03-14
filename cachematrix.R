# The makeCacheMatrix and cacheSolve functions allow the user to
#  cache the inverse of a matrix (assuming the matrix is
#  invertible). If the contents of the matrix
#  haven't changed, these functions look up the cached inverse
#  rather than having to re-perform this potentially time-
#  consuming calculation.


# The makeCacheMatrix function is a closure that defines functions to
#  set (cache) and retrieve matrices and their inverses.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  # Set m equal to NULL in the defining environment of the below functions
  #  (set, get, setinv, and getinv); this controls the value of m in the
  #  below functions under lexical scoping, allowing m to be different when
  #  these functions are called for different matrices x.
  
  set <- function(y) {
  # Define function "set", which takes argument y (an invertable matrix)
    
    x <<- y
    # Set x to "equal" the argument value y when makeCacheMatrix is called
    #  with a matrix argument. x is the "special matrix" (i.e. not exactly
    #  the same as a matrix object in enclosing environments).
  
    m <<- NULL
    # Initialize m (cached value of the inverse of matrix x) in the
    #  enclosing / parent environments (through use of the "<<-" operator)
  }
  
  get <- function() x 
  # This function will return a matrix equal to whatever x was last
  #  set to by the "set" function above
  
  setinv <- function(solve) m <<- solve
  # setinv will cache the argument, storing it in variable m (associated
  #  with a given matrix x).
  #  The argument passed to setinv should be the inverse of x, when called
  #  by cacheSolve below.
  
  getinv <- function() m
  # getinv returns m, the cached inverse of the matrix x
  
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
  # makeCacheMatrix returns the functions "set", "get", "setinv", and
  #  "getinv".
}

# The cacheSolve function checks if the inverse of a special matrix x
#  (passed in the arguments) already exists; if it does, the cached inverse
#  of x is returned; if it does not exist yet, then the inverse is computed
#  and the inverse is "cached" (stored in m) for later retrieval.

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  # for matrix x, use the getinv function defined above to retrieve the
  #  cached inverse matrix m associated with x.

  if(!is.null(m)) {
    # This condition is true if there is already a cached inverse for x
    
    message("getting cached data")
    return(m)
    # Return the inverse matrix already cached for x, and leave the
    #  cacheSolve function
  }
  
  # If the condition above is false (i.e. if there is not already a
  #  cached inverse for x), then execute the following to compute the
  #  inverse and cache the solution:
  
  data <- x$get()
  # retrieve the matrix x and store it in a matrix called "data"
  
  m <- solve(data, ...)
  # compute the inverse of the matrix
  
  x$setinv(m)
  # Call setinv to save (cache) the inverse of x in m
  
  m
  # Return a matrix that is the inverse of 'x'
}
