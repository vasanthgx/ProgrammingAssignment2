## A R Program for cacheing time consuming computations, by preserving the state inside a R Object
## There are two functions in program..1) makeCacheMatrix()  and ..2) caschesolve()

## Definition of makeCacheMatrix() function..This function creates a special matrix object that can cache its inverse
## This function takes a matrix function as its argument 
## m is initialized to null value
## we define a set() function which uses the `<<` operator to assign the values in the 
##defining environment ( making use of lexical scoping). The value of Y is assigned to x and NULL to m within the function

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setmatinverse <- function(solve) m <<- solve
   getmatinverse <- function() m
   list(set = set, get = get,
        setmatinverse = setmatinverse,
        getmatinverse = getmatinverse)
 }

## Write a short comment describing this function

cachesolve <- function(x, ...) {
   m <- x$getmatinverse()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatinverse(m)
   m
 }
 
