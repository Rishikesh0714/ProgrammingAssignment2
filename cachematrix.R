## Made this functions after understanding the example functions of mean given 
## in the question

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
        x <<- y
        mat <<- NULL
}
  get <- function() x
  setinv <- function(solve) mat <<- solve
  getinv <- function() mat
  a <- c(set = set,get = get)
  b <- c(set = setinv,getinv = getinv)
  cbind(a,b)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x[[2,2]]()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x[[2,1]]()
  mat <- solve(data, ...)
  x[[1,2]](mat)
  mat
        
  ## Return a matrix that is the inverse of 'x'
}
