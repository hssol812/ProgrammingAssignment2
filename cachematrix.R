## Put comments here that give an overall description of what your
## functions do

# Sample use
# A<-c(1:4)
# dim(A)<-c(2,2)
# tmp<-makeCacheMatrix(A)
# inv_out<-cacheSolve(tmp)

## Write a short comment describing this function

# makeCacheMatrix creates a list of functions that
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_in) inv <<- inv_in
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
# This function checkes if inverse is already cached, if not it calculates and caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
