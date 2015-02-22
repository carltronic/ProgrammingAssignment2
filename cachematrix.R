## carltronic - February 22, 2015
## Final submission for Programming Assignment 2

## Below are two functions that are used to create a special object that stores
## a matrix and caches its inverse.
## The matrix is assumed to be an n-by-n square invertible matrix. 


## The first of two functions, makeCacheMatrix, creates a special "matrix" object.
## The special object is a list of functions that perform the following:
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the inverse matrix
## 4.  get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse
  inverse <- NULL
  
  ## set() caches the matrix and sets the cached inverse matrix to NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
 
  ## get() returns the cached matrix
  get <- function() x
  
  ## setinverse() caches the inverse of the matrix
  setinverse <- function(i) inverse <<- i
  
  ## getinverse() returns the cached inverse of the matrix
  getinverse <- function() inverse
  
  ## returns a list of the four functions associated with the matrix object
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second of the two functions, cacheSolve, calculates the inverse 
## of the special "matrix" object created with the makeCacheMatrix function.
## The solve() function is used to caculate the matrix inverse. 
## As stated earlier, the matrix is assumed to be an n-by-n square
## invertible matrix. 
cacheSolve <- function(x, ...) {
  
  ## If the inverse has already been calculated, the cached inverse matrix
  ## is returned.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
 
  ## There is no cached inverse matrix.
  ## The inverse is calculated, cached and returned.
  
  ## The matrix is retrieved from the special "matrix" object.
  data <- x$get()
  
  ## The inverse matrix is calculated using the solve() function.
  inverse <- solve(data, ...)
  
  ## Caches the calculated inverse matrix for the "matrix" object.
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
