## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){## set the value of the matrix
    x<<-y
    m<<-NULL
  }
  get<-function() x ## get the value of matrix
  setmatrix<-function(solve) m<<- solve ## set the output of the matrix inverse
  getmatrix<-function() m ## get the output of the matrix inverse
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache. Otherwise, it will retrieve the special "matrix" and computes the inverse and stores the output in the cache via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getmatrix() 
  if(!is.null(m)){## If matrix inverse has been computed, output is provided via the cache and computation skipped
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() ## Otherwise, get value of the matrix
  m<-solve(matrix, ...) ## Compute the inverse
  x$setmatrix(m) ## Set the matrix inverse output in the cache via the setmean function
  m
}
