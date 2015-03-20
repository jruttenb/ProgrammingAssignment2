# create a matrix to be inverted and cached
makeCacheMatrix <- function(x=matrix(), nrow=2, ncol=2) {
  # define empty variable
  m <- NULL
  
  set <- function(y) {
    # force x in the current environment to be y
    x<<-y
    
    # force m in the current environment to be empty
    m<<-NULL
  }
  get <- function() x
  
  # set value of solve to be m in the makeCacheMatrix frame
  setsolve <- function(solve) m <<- solve
  
  # return value of m from the makeCacheMatrix frame
  getsolve <- function() m
  
  # functions in the makeCacheMatrix frame
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# invert the matrix and store it for future use
# if the matrix has already be cached, return the cache value
cacheSolve <- function(x, ...) {
  
  # return a matrix into m that is the inverse of x
  m <- x$getsolve()
  
  # if the matrix is cached, return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  
  # if the matrix is not cached, get the matrix from x
  data <- x$get()
  
  # calculate the inverse
  m <- solve(data, ...)
  
  # set the inverse matrix to x
  x$setsolve(m)
  
  # return the inverse matrix
  m
}