makeCacheMatrix <- function(x = matrix()) {
  m<-NULL   
  evn <- environment()  
  y<-NULL 
  setmatrix<-function(y){
    x<<-y  
    m<<-NULL 
  }
  getmatrix<-function() x  
  setinverse<-function(solve) m<<- solve  
  getinverse<-function() m  
  getenv<- function() environment()
  list (setmatrix=setmatrix, getmatrix = getmatrix,   
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

cacheSolve <- function(xMat= m(), ...) {
  ## Return a matrix that is the inverse of 'x'
  # Run function e.g. like this: minv<-cacheSolve(xMat = m)
  # Compares matrix to what was there before!
  m <- xMat$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(xMat$setmatrix() == xMat$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    # otherwise 
    y <- xMat$getmatrix() 
    xMat$setmatrix(y) 
    m <- solve(y, ...) 
    xMat$setinverse(m) 
    m # return the inverse
  }
  
}
