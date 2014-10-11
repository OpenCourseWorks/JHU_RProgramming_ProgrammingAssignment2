## Programming assignment 2 by Wei-Cheng Yang
## makeCacheMatrix: make a matrix capable of
## set the value of matrix, get the value,
## get inverse and set inverse
## modified on the basis of makeVector


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}
