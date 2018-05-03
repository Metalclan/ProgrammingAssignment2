## Put comments here that give an overall description of what your
## functions do

## ## This function creates a matrix that is actually a list with functions  
## to set/get the matrix and set/get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
} 

## This function checks if the inverse of x is in in the cache. 
## If it is, and x has not changed, it reads the inverse from the cache an returns that value 
## if it is not, it calculates the inverse using the function 'solve', and then stored it in the cache. 


cacheSolve <- function(x, ...){  
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