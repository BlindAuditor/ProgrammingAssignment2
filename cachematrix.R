## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix"object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  old <- NULL ## This is an empty matrix to store input from last time
  get <- function() x ## This function gives the value of input matrix
  set <- function (y) { ## This function sets new value to the old cached matrix
    x <<- y
  }
  getinverse <- function() i ## This function gives the cached inverse matrix
  setinverse <- function(inv){ ## This function sets value to the cached inverse matrix
    i <<- inv
    old <<- x ## Additional line to store the input of last test
  }
  checkold <- function() old
  
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse, checkold = checkold)
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse () ## This gives the cached inverse matrix stored in makeCacheMatrix, if any. 
        
  if (!is.null(i)){
    if(identical(x$get(),x$checkold())){ ## Check if input is the same as last time
    message("getting cached data")
    return(i)## Return a matrix that is the inverse of 'x' 
    }
  }
  matrix <- x$get() ## Get matrix from original input
  i <- solve(matrix) ## Calculate the inverse of matrix
  x$setinverse(i) ## Store the calculated inverse in the makeCacheMatrix function
  i
}
