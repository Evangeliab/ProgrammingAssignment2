## Assignment: CACHING THE INVERSE OF A MATRIX

# Overall, the pair of functions caches the inverse of a matrix and return it.
# Thus, if the same matrix is called again through the function, its output that  
# has been stored will promptly return (instead of being computed from scratch). 
# Note: It's assumed that the matrix supplied is always invertible.



## The "makeCacheMatrix" function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL         ## it's the special object.
        set <- function(y) {
                x <<- y       ## sets its input as 'x'. 
                s <<- NULL    ## s is reseted to NULL
        }  
        get <- function() x      ## returns the value of the original matrix.
        setsolve <- function(solve) s <<- solve    ## stores the inverse matrix.
        getsolve <- function() s     ## returns the cached value.
        list(set = set, get = get,     # a list of the internal functions, that
             setsolve = setsolve,      # assists the calling function to access  
             getsolve = getsolve)      # those 4 functions.
}

 

## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()   ## accesses the cached s of the particular 'x' above. 
        
        if(!is.null(s)) {            # If the inverse has already been calculated             
                message("getting cached data")    # then the cachesolve should 
                return(s)            # retrieve the inverse from the cache.'if' prints       
        }                            # a message and the value,then exits the function.
        
        data <- x$get()         ## accesses the original matrix.
        s <- solve(data, ...)  ## calculates the inverse matrix.
        x$setsolve(s)        ## stores the calculated value in the cache.
        s                   ## returns the inverse matrix of 'x'.
}
