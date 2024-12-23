# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {
    x <<- y  
    inv <<- NULL  
  }
  get <- function() x  
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv  
  
 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  
  }
  mat <- x$get()  
  inv <- solve(mat, ...) 
  x$setInverse(inv)
  inv
}

matrixObject <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))

cacheSolve(matrixObject)

cacheSolve(matrixObject)

matrixObject$set(matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2))

cacheSolve(matrixObject)
