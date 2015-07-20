##creates a special "matrix", containing functions to:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inversed matrix
##4.  get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
      inversedMatrix <- NULL
      set <- function(newMatrix) {
            ##setting x as the new matrix & deleting old inverse calculations
            x <<- newMatrix
            inversedMatrix <<- NULL
      }
      get <- function(){ x }
      setInverse <- function(inverse) { inversedMatrix <<- inverse }
      getInverse <- function() { inversedMatrix }
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

##calculates the inverse of the special "matrix", The function first checks to see if the inverse has 
##already been calculated. If so, it `get`s the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
##the `setInverse` function.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inversedMatrix <- x$getInverse()
      if(!is.null(inversedMatrix)) {
            ##getting cached data
            return(inversedMatrix)
      }
      else{
            ##no cached data - recalculating
            matrix <- x$get()
            inversedMatrix <- solve(matrix)
            x$setInverse(inversedMatrix)
            inversedMatrix
      }
}