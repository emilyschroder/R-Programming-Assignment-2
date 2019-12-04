##Programming Assignment 2: Lexical Scoping
##Emily Schroder

##1 makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##(For this assignment, assume that the matrix supplied is always invertible.)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##Above creates objects x and i
  
  ##resets/clears cache for x and i if needed
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##get and set functions
  get <- function() x
  setInverse <- function(source) i <<- source
  getInverse <- function() i
  
  ##assigns each of these functions as an element of a list
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {#x must be an environment created through makeCacheMatrix
  i <- x$getInverse()
  
  ##if i not null then the data has been cached - pull from cached data
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##if i is null (not cached) then the inverse will be caluclated through the set and get functions
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}



##Test it out!
##First run will be uncached
##Second run will be from cache and will generate the "getting cached data" message

mat<-matrix(sample(9),3,3) 
results<-makeCacheMatrix(mat)

cacheSolve(results)
cacheSolve(results)


