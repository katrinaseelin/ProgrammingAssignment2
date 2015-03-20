## MakeCacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL              ## xinv is where the result of the matrix invertion will be stored
  set <- function(y) {      
    x <<- y
    xinv <<- NULL           ## initializes xinv to NULL

}
get <- function() x         ## return the input matrix
setinv <- function(inv) xinv <<- inv     ## set the inverse matrix
getinv <- function() xinv                ## return the inverse matrix
list( set = set, get = get, setinv = setinv , getinv = getinv)
}

x = rbind(c(1, -1/4), c(-1/4, 1))        ## test data
xinv = makeCacheMatrix(x)
xinv$get()

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated -- and the matrix has not changed -- should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  yinv <- x$getinv()                ## obtain inverse matrix from object x 
                                    ## yinv will be NULL if uncalculated
  if(!is.null(yinv)) {              ## if the inversion result exists, 
    message("Getting Cached Data")
    return(yinv)                    ## return the inverse matrix
  }
  data <- x$get()                   ## if the inversion result does not exist
  yinv <- solve(data)               ## solve data
  x$setinv(yinv)
  yinv
}
        
cacheSolve(xinv)                    ## run cacheSolve function

