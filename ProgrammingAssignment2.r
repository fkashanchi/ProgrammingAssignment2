##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
 
makeCacheMatrix <- function(original.matrix = matrix()) { 
 
 if (!is.matrix(original.matrix)) { 
    stop("Please give a matrix") 
  } 
   
  inverted.matrix <- NULL 
   
   set <- function(y) { 
     original.matrix <<- y 
    inverted.matrix <<- NULL 
  } 

   get <- function() original.matrix 
  set.inverse <- function(solve) inverted.matrix <<- solve 
   get.inverse <- function() inverted.matrix 
    
   list( 
    set = set,  
    get = get, 
    set.inverse = set.inverse, 
    get.inverse = get.inverse) 
    
 } 
 
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheable.matrix, ...) { 
inverted.matrix <- cacheable.matrix$get.inverse() 
  
  if(!is.null(inverted.matrix)) { 
    message("Getting cached inverse matrix") 
     return(inverted.matrix) 
   } 
 
   matrix.to.inverse <- cacheable.matrix$get() 
   inverted.matrix <- solve(matrix.to.inverse) 
  cacheable.matrix$set.inverse(inverted.matrix) 
   inverted.matrix 
    
 } 
