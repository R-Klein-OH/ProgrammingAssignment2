## The function 'makeCacheMatrix' below performs the following tasks:
##    sets the value of the matrix,
##    gets the value of the matrix,
##    uses the solve() function to set the value of the inverse of the matrix,
##    and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## The 'cacheSolve' function below returns the inverse of the input matrix generated with the 'makeCacheMatrix' function above, 
## calculating and saving the inverse if the result is not already available.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

###Testing
#https://www.mathwords.com/i/inverse_of_a_matrix.htm
#myMatrix <- matrix(c(4,3,3,2), 2, 2)
#makeCacheMatrix(x = myMatrix)
#cacheSolve(makeCacheMatrix(x = myMatrix))

#myMatrix
#solve(myMatrix)

#identical(cacheSolve(makeCacheMatrix(x = myMatrix)), solve(myMatrix))
