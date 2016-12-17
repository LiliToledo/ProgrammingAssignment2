
makeCacheMatrix <- function(x = matrix()) {
  inve = NULL 
  set <- function(y){
    
    x <<- y
    inve <<- NULL
    
  }
  get = function() x
  setinve = function(inverse) inve <<- inverse
  getinve = function() inve
  list(set=set, get=get, setinve=setinve, getinve=getinve)

}
cacheSolve <- function(x, ...){
  inve = x$getinve()
  if (!is.null(inve)){
    message("getting cached information")
    return(inve)
  }
  mat.data = x$get()
  inve = solve(mat.data, ...)
  x$setinve(inve)
  return(inve)
  }
