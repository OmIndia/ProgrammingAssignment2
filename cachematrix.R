## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix input, checks that it is not a singular matrix, then using set/get 
## functions saves/retrieves the matrix and inverse in/from parent environment.

makeCacheMatrix <- function(x = matrix) {
  
   inverse <- NULL
    set <- function(y) {
      if(det(y)==0) {
        message("Enter non singular matrix")
        return("Error..try again")
      }
      x<<-y
      inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  



## Write a short comment describing this function
## The function first checks whether the inverse of the matrix is existing. 
## If so, retrieves it from parent environment. if not, creates it using "solve" R function and 
## using set function of makeCacheMatrix, stores the inverse in parent environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
  
}
