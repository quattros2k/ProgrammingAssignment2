## This program contais a pair of functions that cache the inverse of a matrix.
## 

## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL   ## initiates "inv" as an empty object within this function
  set <- function(y) {   ## set value of matrix
    x <<- y  ##  puts arguments of set function into global object x
    inv <<- NULL  ##initiates "inv" as empty global object 
  }
  get <- function() x   ## get value of matrix
  setinv <- function(inverse) inv <<- inverse   ##set value of inverse
  getinv <- function() inv  ## get value of inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
 
  
  inv <- x$getinv()
  if(!is.null(inv)) {   ##checks to see if inverse already calculated, if so, gets inverse from cache, skips computation
    message("getting cached data")
    return(inv)  ## returns inverse if cached
  }  ## the following computes inverse and sets it in cache
  data <- x$get()
  inv <- solve(data, ...)  ##Generates the inverse of the matrix, stores in "inv" object
  x$setinv(inv)  ##sets inverse in cache
  inv  ## returns inverse if not cached
  
  

  }


## to run this, pass an invertible square matrix to makeCacheMatrix(), while assigning makeCacheMatrix() to a variable.   
## run, then, pass that variable to cacheSolve()

