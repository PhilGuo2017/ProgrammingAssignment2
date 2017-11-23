## Put comments here that give an overall description of what your
## functions do

## This is a function to create a likst containing a function to 
##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse of this matrix
##4) get the value of inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
        ##inv is looked up in the parent level, reset inv if x is reset
    }
    ##set the value of matrix
    get <- function() x
    ##get the value of matrix
    setinverse <- function(inverse) inv<<- inverse
    ##set the value of inverse of the matrix
    getinverse <- function() inv
    ##get the value of inverse of the matrix
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}      


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse() 
          ##get the cached data from makeCacheMatrix
          if(!is.null(inv)){
                  message("getting cached data")
                  return (inv)
          }
          ##check if there is cached inverse, then return this inverse matrix
          data <- x$get()
          ##get the matrix values
          inv<-solve(data,...)
          ## calculate the inverse of the matrix
          x$setinverse(inv)
          ##set the inverse
          inv
          ##return the inverse value
}
