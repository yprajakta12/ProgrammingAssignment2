makeCacheMatrix <- function(x = matrix()) {
  j <- NULL            #initializing j as NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x   #function to get matrix x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j   #function to obtain inverse of matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) #gets cache data
  { 
  
  j <- x$getInverse()   
  if(!is.null(j)){
    message("getting cached data")
    return(j)         #return inverse value
  }
  mat <- x$get()
  j <- solve(mat,...)  #calculates inverse value
  x$setInverse(j)
  j      #Return a matrix that is inverse of x
}
