## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  ptm <- proc.time()
  
  ## Get Matrix "mat" from Cache
  
  mat <- x$getinv()        
  
  ## Check if not null, Retrun value read from Cache
  
  if(!is.null(mat)) {
    message("getting cached data")
    cat("Process Time", proc.time() - ptm)
    return(m)
  }
  
  ## if "mat" is NULL, get origal Matirx
  
  mydata <- x$get()
  
  ## Calculate inverse using Solve
  
  mat <- solve(mydata, ...)
  x$setinv(mat)
  cat("Process Time", proc.time() - ptm)
  
  ## Returm inverse
  
  mat
  
}
