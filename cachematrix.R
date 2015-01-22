## These functions provide an example of using cache 
## to store a data vaule for use at a later time.

## makeCachdMatris is a function that creates a
## list of functions and data stores in cache
## for retrieval by the cacheSolve function.

## input to the function is a matrix for which the 
## inverse is desired.

makeCachdMatrix <- function(x = matrix()) 
    {
    m <- NULL
    set <- function(y) 
        {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    print(getinverse)
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    }


## cacheSolve(x) returns the inverse matrix of 
## of the matrix input into the makeCachdMAtrix
## function.

## input to the function is the makeCachdMAtrix
## function with the original matrix data.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}