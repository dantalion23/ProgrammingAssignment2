## A pair of functions to compute the inverse of a matrix and 
## cache the result so that the result does not have to be 
## re-computed if the matrix hasn't changed since the inverse 
## was last computed.

## Function to set-up the cache and functions to handle the cache.

## Usage: cc <- makecacheMatrix(A)
## where: A is a matrix.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinv <- function(inv) m <<- inv
   getinv <- function() m
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to compute the matrix inverse OR return the cached inverse, as appropriate.

## Usage: cacheSolve(cc)
## where: cc is the object returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)){
    message("Getting cached inverse.")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat)
  x$setinv(m)
  m
}
