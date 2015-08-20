
## Creates a special matrix object that can cache its inverse

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function(){
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) {
    
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getinverse <- function(){ 
    
    ## Return the inverse property
    
    inv
  }
  
  ## Return a list of the methods in the function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## The function assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  ## Just return the inverse if its already set
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse : use solve() to get the inverse matrix
  inv <- solve(data)
  
  ## Set the inverse to the object
  x$setinverse(inv)
  
  ## Return the matrix
  inv
       
}


## Sample test

## x= matrix(c(1,3,4,6),2,2)
## m = makeCacheMatrix(x)
## m$get()

## [,1] [,2]
## [1,]    1    4
## [2,]    3    6

## No cache in the first run
## cacheSolve(m)

## [,1]       [,2]
## [1,] -1.0  0.6666667
## [2,]  0.5 -0.1666667

## Retrieving from the cache in the second run

## cacheSolve(m)
## getting cached data.
##       [,1]       [,2]
## [1,] -1.0  0.6666667
## [2,]  0.5 -0.1666667