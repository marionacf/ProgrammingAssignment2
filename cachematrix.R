## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" objec that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Clear the cache and Set the value of the matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function(){
                x
        }
        ## Set the inverse of the matrix
        setInverse <- function(inverse){
                inv <<- inverse
        } 
        
        ## Get the inverse of the matrix
        getInverse <- function(){ 
                inv
        }
        
        ## Return a list of the methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## Checks if the cache memory is emty. If not, returns the inverse matrix
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If the cache memory is emty, calculate the inverse matrix
        ## Get the value of matrix
        mat <- x$get()
        
        ##Calculate the inverse matrix
        inv <- solve(mat, ...)
        
        ## Memorize the result in the cache
        x$setInverse(inv)
        
        ## Return the inverse of the matrix
        inv
}