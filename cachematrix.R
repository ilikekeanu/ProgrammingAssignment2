## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        ## assign m with value NULL
        
        set <- function(y) {
                ## Set is a function that changes the vector 
                ## stored in the main function
                
                x <<- y 
                ## Substitutes the vector x with y (the input) 
                ## in the main function makeCacheMatrix
                
                m <<- NULL
                ## Restores to null the value of the inverse m, 
                ## because the old inverse of the old vector is 
                ## not needed anymore. The new inverse needs to
                ## be recalculated through the function cacheSolve
        }
        
        get <- function() x
        ## Returns the vector x stored in the main function
        
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        ## They don't calculate the inverse, they simply store 
        ## the value of the input in a variable m into the main 
        ## function makeCacheMatrix (setsolve) and return it (getsolve)
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        ## To store the 4 functions in the function makeCacheMatrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## The first thing cacheSolve does is to verify the value m, 
        ## stored previously with getsolve, exists and is not NULL. 
        ## If it exists in memory, it simply returns a message and the value m, 
        ## that is supposed to be the inverse, but not necessarily.
        ## If it was the case, "return(m)" would have ended the function. 
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## This part is like a ELSE to the previous IF. data gets the vector 
        ## stored with makeCacheMatrix, m calculates the inverse of the vector 
        ## and x$setsolve(m) stores it in the object generated assigned 
        ## with makeCacheMatrix.      
        
}