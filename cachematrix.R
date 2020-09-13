## Put comments here that give an overall description of what your
## functions do


## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL
  set <- function(y) {
    x <<- y
    iM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) iM <<- inverse
  getInverse <- function() iM
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iM <- x$getInverse()
  if (!is.null(iM)) {
    message("getting cached data")
    return(iM)
  }
  mat <- x$get()
  iM <- solve(mat, ...)
  x$setInverse(iM)
  iM
}
APmatrix<- makeCacheMatrix(matrix(c(2,3,1,4), 2,2))
APmatrix$get() #retrieve the value of X
APmatrix$getInverse() #retrieve the value of iM
cacheSolve(APmatrix) #retrieve the inverse Matrix
APmatrix$getInverse()
