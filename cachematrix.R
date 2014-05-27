## This function will save off (cache) the inverse of a matrix
## It contains the functions
##  Set - intialize the matrix and clear the cached inverse
##  Get - returns a cached inverse
##  setinverse - puts the inverse into the cache
##  getinverse - gets the inverse from the cache


makeCacheMatrix <- function(x= matrix(1:1,1)) {
  ##
  ## This is where we need to initialize m 
  ##  and check for the change in the matrix
  ##  if just leave this here will always
  ##  initialize to null
  
  ## this will see if m exists and if it doesn't
  ##  it will initialize the varible for use
  
  ## REVIEW COMMENT -->
  ## The exists check it not necessary here because this part of 
  ## the function is only executed when a new matrix object is 
  ## created (and then it's fine to set it to NULL).
  ## <-- REVIEW COMMENT
  if (!exists("m")){
    m <<- NULL
  }
  
  ## This will check to see if the current matrix has the same number
  ##  of rows and columns as the previous matrix.  This is important
  ##  because if they are different sizes there, the all() function
  ##  creates an error.  Also indicates it is not the same matrix.
  ##  If they are different sizes, they are different, so initialize
  ##  m.
  ## First check to see if the previous matrix exists.  If it does, check it
  ##  If it doesn't, set m
  
  ## REVIEW COMMENT -->
  ## You could use the identical() function here to check whether x and lstmat 
  ## are the same.
  ## A better place to put this check would have been the set function.
  ## <-- REVIEW COMMENT
  if(exists("lstmat")) {
    if (nrow(x)!=nrow(lstmat) | ncol(x)!=ncol(lstmat)){
      m <<- NULL
    }
    else {
      ## This will check the previous matrix with the current
      ## matrix.  If they are not the same, clear the varible m
      if (!all(x == lstmat)) {
        m <<- NULL
      }
    }
  }
  else {
    m <<- NULL
  }
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  ##      setmat <- function(x) lstmat <<- x
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will caclulate the inverse of a matrix 
##  or use the value cached if the matrix has not changed

cacheSolve <- function(x, ...) {
  ## get the inverse 
  m <- x$getinverse()
  
  ## if the value is not null, the use the value and exit
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the code got here, then the value was null
  ## calculate the inverse
  ## and set the values for later use
  data <- x$get()
  
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## This will save the matrix just processed to compare against the next one
  ## REVIEW COMMENT -->
  ## The lstmat value set here will not be accessible in makeCacheMatrix because this is 
  ## not the parent environment for makeCacheMatrix!
  ## <-- REVIEW COMMENT
  lstmat <<- data
  
  ## return the inverse
  m
}

