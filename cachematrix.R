## Put comments here that give an overall description of what your
## functions do

## This creates a list that sets the value of the matrix to be evaluated, gets the value of that matrix, sets the value of the inverse, and then gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
}
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This calculates the inverse of the matrix list created with the above function. It first checks to see if the inverse has already been calculated and stored. 

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}
