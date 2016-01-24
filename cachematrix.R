## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## This function creates a list containing functions that
## allow to access and modify a matrix and its inverse.
## This function caches the value of the inverse (variable i)

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    
    x <<- y
    i <<- NULL
    
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list (set = set, get = get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
## This function calculates the inverse of a matrix and stores
## it in the list created by the function  makeCacheMatrix.
## If the inverse has just been calculated, the inverse is not
## calculated again and it is restored from the cache.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if (!is.null(i)){
    
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
  
}