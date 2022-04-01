## Below are a pair of functions that are used to create a special matrix
## Populate and cache the inverse of a matrix to avoid computing repeatedly 


## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## makeCacheMatrix contains a list of four functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {    
          x <<- y              
          inv <<- NULL         
     }
     get <- function() x       
     setinv <- function(inverse) inv <<- inverse 
     getinv <- function() inv  
     list(set = set, get = get,  
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix: find it in the cache, exist then return
## if not in the cache, then calculate inverse and set this value to inv in the parents environment

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)      # calculate inverse value
     x$setinv(inv)
     inv
}
