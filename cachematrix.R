## Author: Melany Echeverria
## Create the special matrix to compute the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Compute the inverse of a square matrix and saves it in cache the first time
## If the result is already in cache, return the saved data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## End.process shows the time elapsed calculating the inverse
  ## This is to show the time we save using this experiment
  Start.process <- Sys.time()
  i <- solve(data, ...)
  End.process <- Sys.time()
  Exec.time <- End.process - Start.process
  print(Exec.time)
  x$setsolve(i)
  i
}
