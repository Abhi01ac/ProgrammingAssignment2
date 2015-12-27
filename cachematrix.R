## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates  a special object that genarates a matrix and cache's its inverse.
makeCacheMatrix <- function(x = integer(), nrow = 2, ncol = 2) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     
     xx1 <- matrix ( x, nrow = nrow, ncol = ncol)
     
     getmatrix <- function () xx1
     
     setinvmatrix <- function(solve) m <<- solve
     
     getinvmatrix <- function() m
     
     list(set = set, getmatrix = getmatrix 
          , setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
     
 }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m <- x$getinvmatrix()
    
    if(!is.null(m)) {
        message("Retriving Inverse from Cache")
        return(m)
    }
    
    data <- x$getmatrix()
    
    m <- solve(data,...)
    
    x$setinvmatrix(m)
    
    m
}
