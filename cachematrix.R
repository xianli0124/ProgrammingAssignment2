# Function to create a special matrix object that can cache 
# its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                  # Declaring the inverse matrix
  set <- function(y) {       # Storing the matrix and its inverse
    x <<- y
    m <<- NULL
  }
  get <- function() x        # Declaring getter and setter functions
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to solve the special matrix object with the cached inverse
# Retreives the inverse if it was already stored or sets the inverse cache
cacheSolve <- function(x, ...) {
  m <- x$getInverse()  # Get the cached data and checking to see if it exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()      # If inverse not cached yet, find the inverse and store it
  m <- solve(data, ...)
  x$setInverse(m)
  m                    # Return the inverse of the matrix
}
