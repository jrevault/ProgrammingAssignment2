## This function caches the inverse of a matrix.
## It takes a matrix as an arguments
## and offers function to 'get' and 'set' the
## solved matrix of the given matrix
makeCacheMatrix <- function( x = matrix() ) {

  # We initiate the solved matrix to a NULL
  sm <- NULL
  # We declare a function that caches into a different environment
  # Setter function for the original  matrix
  set <- function( y ) {
    x <<- y
    sm <<- NULL
  }
  # Getter function for the original matrix
  get <- function() x
  # Setter function  for the solved matrix
  setinverse <- function( solve ) sm <<- solve
  # Getter function for the solved matrix
  getinverse <- function() sm
  # Prepares the abilities over the current object
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse )
  
}

## This function returns th inverse of a square invertible matrix.
## It takes a matrix as an arguments, as well as all the arguments the
## 'solve' function could take
## This function calls the 'getinverse' solved matrix on the current matrix
## in order to get it if it has already bean computed
## if it hasn't, it solves the matrix and save it into cache via th 
## 'setinverse' function. After that it just returns the solved matrix
cacheSolve <- function(x, ...) {
  # Gets the matrix that is the inverse of 'x' if it was already computed
  sm <- x$getinverse()
  # If we have an inverse matrix in cache
  if( ! is.null( sm ) ) {
    # Print message
    message( "getting cached data" )
    # returns the solved matrix from cache
    return( sm )
  }
  # Otherwise we get the data
  data <- x$get()
  # then compute the inverse of the matrix
  sm <- solve( data , ... )
  # Store the inverse matrix inside cache
  x$setinverse( sm )
  # And return the newly computed solved matrix
  sm
}
