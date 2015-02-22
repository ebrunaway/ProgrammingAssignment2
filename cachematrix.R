## This function creates a special "matrix" object 
## that can cache its inverse.
## 
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set he value of the inverse
## getinverse - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # support variable, initial value is NULL
  tmp <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    tmp <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set he value of the inverse
  setinverse <- function(inverse) tmp <<- inverse
  
  ## get the value of the inverse
  getinverse <- function() tmp
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  # returns the inverse if it's already cached
  if(!is.null(i)) return(i)
  
  # computes and returns the inverse of the matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
