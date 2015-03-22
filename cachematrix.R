
## This function creates a vector which is a list that:
##   sets the value of the vector
##   gets the value of the vector
##   sets the value of the inverse of the matrix
##   gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  
  
  m<-NULL
  set <- function(y){
        x << - y
        m <<- NULL
    
  }

  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix
       ,getmatrix=getmatrix)
}


##This function calculates the inverse of the matrix created with the MakeCacheMatrix.
##It first checks to see if the inverse of the matrix has already been calculated;
##  if so, it gets the inverse value from the cache and skips the calculation. If not,
##  it calculates the inverse of the matrixand sets the value of the inverse in the catch
##  via the setmatrix function. 

cacheSolve <- function(x, ...) {
  m <-x$getmatrix()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
