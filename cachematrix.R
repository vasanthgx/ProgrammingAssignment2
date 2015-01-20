## The following code is a  R Program for cacheing time consuming computations, by preserving the state inside a R Object
## There are two functions in program..1) makeCacheMatrix()  and ..2) caschesolve()

## Definition of makeCacheMatrix() function..
##-----------------------------------------------

##This function creates a special matrix object that can cache its inverse
## This function takes a matrix () function as its argument 
## m is initialized to null value
## we define a set() function which uses the `<<` operator to assign the values in the 
##defining environment ( making use of lexical scoping). The value of Y is assigned to x and NULL to m within the function
## if suppose a<- makeCacheMatrix(matrix(c(1,0,5,  2,1,6,  3,5,0),nrow=3, ncol=3)) is computed
## a$get() gives us the input matrix
## cachesolve(a)...This is the second function, gives us the inverse of the matrix..which is the cached data.
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


## Definition of cachesolve() function
##--------------------------------------
## This is the second function which returns the cached data. That is the inverse of the matrix
## The inverse of the matrix is calculated by the usage of the solve() function
## after computing the cachesolve(), the a$getmatinverse() returns the inverse of the matrix. That is the cached data
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
 


####...example of runing the above code(optional)
##----------------------------------------------------

a<- makeCacheMatrix(matrix(c(1,0,5,  2,1,6,  3,5,0),nrow=3, ncol=3))## Taking a 3 by 3 matrix as an argument for the makeCacheMatrix() function

> a$get() ## Checking the input matrix

      [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    5
[3,]    5    6    0


> cachesolve(a) ## The below is the desired output ie, it is the inverse of the input matrix


       [,1] [,2] [,3]
[1,]   -6  3.6  1.4
[2,]    5 -3.0 -1.0
[3,]   -1  0.8  0.2


> a$getmatinverse()

      [,1] [,2] [,3]
[1,]   -6  3.6  1.4
[2,]    5 -3.0 -1.0
[3,]   -1  0.8  0.2
>