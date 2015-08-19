
## This function takes in an input of an invertable matrix x and gets/sets the value of its inverse. 

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

## This function calculates the inverse of a matrix, but will first check if it has already been calculated.
## If already calculated, the function will return the cached inverse value + a comment

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
    
    if(!is.null(inv)) { #Check if the inverse value is stored in the cache and fetch the value 
      message("....Retrieved cached values...")
      return(inv)
    }
  
    else { #If inverse not stored in cache, calculate it 
      matrix <- x$get() #fetch the data for the matrix 
      inv <- solve(matrix)
      x$setinv(inv) # cache the inverse 
      return (inv) 
    }
}
