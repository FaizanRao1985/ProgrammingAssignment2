## Put comments here that give an overall description of what your
## functions do
## The way to call the functions at command line, is to first assign using makeCacheMatrix, and then call
##cacheSolve by passing the above as argument
## The two functions below will cache a matrix inverse, and check to see if the matrix is present.

## Write a short comment describing this function
## makeCachematrix() will return a list whose elements are functions to set the input matrix,
## get that matrix, set the inverse of the matrix, and get the inverse ov matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversem <- function(inversem) m <<- inversem
  getinversem <- function() m
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}


## Write a short comment describing this function
## This function checks whether there already exists an inverse of the given matrix, and if not,
## it calculates the inverse, using solve(), and returns that value.

cacheSolve <- function(x, ...) {
  ##x <- makeCacheMatrix()
  m <- x$getinversem()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversem(m)
  m
}
