######################################################
##                                                  ## 
##            INVERTED MATRIX CALCULATOR            ##
##                                                  ##
##  This set of functions calculates the inverse    ##
##  of a matrix and stores the matrix in memory     ##
##  so that it can be retrieved when needed, saving ##
##  processing time.                                ##
##                                                  ##
######################################################

## The makeCacheMatrix() function creates a matrix of 
## functions that are called by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve() function first checks to see if the 
## inverse of the input matrix has already been calculated. 
## If it has been calculated, the function returns the inverted
## matrix.  If it has not been introduced before, the function 
## calculates the inverse of the matrix using the solve() function
## and then stores it in the cache for later retrieval.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#########  EXAMPLE OF HOW IT WORKS.  ###########

#1.Run the above functions

#2. Create some matrixes
x <- matrix(1:6,nrow=2,ncol=2)
y <- matrix(c(2,5,6,7,5,1,6,3,1),3,3)

#3. Run the matrices through the makeCacheMatrix function
a <- makeCacheMatrix(x)
b <- makeCacheMatrix(y)

#4. Run one of them through cacheSolve. You will get an inverted matrix
cacheSolve(a)

#5. Run cacheSolve(a) again. You will still get the inverted matrix with the notification that it was retrieved from the cache. 
cacheSolve(a)

#6. Run the other matrix through cacheSolve. The function recognizes that this is a new matrix and computes a new inverse.
cacheSolve(b)

#7. Finally, run both matrices through cacheSolve again. Notice that both of their values are still cached.
cacheSolve(a)
cacheSolve(b)
