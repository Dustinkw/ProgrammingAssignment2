## Below are 2 functions that can be used to cache 
## the calculation of the inversion of a matrix
## The first function "makeCacheMatrix" takes a matrix and puts it in a special List
## This special List can be passed to the second function "cacheSolve"

##Returns a special list containing 4 functions
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## input: a special list generated from the "makeCacheMatrix" function
## assumption: input includes an invertable matrix retrievable by $get()
## description: Caches the inverse of a matrix in the special list x as described in the above function
## It checks to see if the inverse has already been calculated and stored prior 
## If it has already been calculated and stored, it returns the cached result
## If not, it calculates the inversion of the matrix and stores this into the cache then 
## returns the inverted matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
