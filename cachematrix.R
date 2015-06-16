## makeCacheMatrix takes an invertible matrix as an input and outputs a cached "matrix" 
## consisting of a list of functions from which we can retrive and reassign the original 
## matrix and inverse


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <-function(y) {
    x<<-y
    inv<<-NULL
  }
  
  get<-function () x
  
  setinv <- function(y)   inv<<-y
  
  getinv <-function() inv
  
  list(set = set, get = get, setinv = setinv , getinv = getinv)
}




## If x is a cached "matrix" (i.e. x =makeCacheMatrix(M)), then cachSolve(x)
##is the inverse of the actual matrix M. 

cacheSolve <- function(x, ...) { 
          ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  data <-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}






