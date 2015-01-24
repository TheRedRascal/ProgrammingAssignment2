## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Modification By Santiago Arango Toro , Coursera Course R , January 2015

##The makeCacheMatrix function function according to the next steps
# 1. create an object called mtx with null value, due this mtx will be recovered from the cache or 
##created for the first time
## 2.create a function to set a value according , the set function, stores the matrix on the cache.
## 3.create a function to get the value of the inverse matrix, returing the value
## 4.Create a list with the different operations we just defined to verify and get the matrix if need it.


makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(var)
  {
    x <<- y
    mtx <<- NULL
  }
  
  get <- function() x
  setInverse <- function(sol) mtx<<-sol
  getInverse <- function()mtx
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##the cacheSolve function, checks if the function already exists in the list cache, if so just return
##the existing objet or if not, the method will create it for the first time and store it with the solve function.
# 1. get the matrix from X
# 2. if the object is not null return the cached matriz
# 3. if not, get the input matrix
# 4. solve the matrix inverse
# 5.storet with set inverse, to be ready to use from cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mtx <- x$getInverse()
  if(!is.null(mtx))
  {
    message("getting cached data")
    return(mtx)
  } 
  
  out <- x$get()
  mtx <-solve(out,...)
  x$setInverse(mtx)
}

