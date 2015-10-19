
## Assignment: Caching the Inverse of a Matrix


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(matr = matrix()) 
{
    inv_matr <- NULL
    
    set_matrix <- function(y) 
    {
        matr <<- y
        inv_matr <<- NULL
    }
    
    get_matrix <- function() matr
    
    set_inverse_matrix <- function(z) inv_matr <<- z
    get_inverse_matrix <- function() inv_matr
    
    list(set = set_matrix, get = get_matrix, 
         set_inv = set_inverse_matrix, get_inv = get_inverse_matrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    inv_matr <- x$get_inv()
    
    if(!is.null(inv_matr)) 
    {
        message("getting cached data")
        return(inv_matr)
    }
    
    data <- x$get()
    inv_matr <- solve(data, ...)
    x$set_inv(inv_matr)
    
    inv_matr
}