## The program is written by Jia Junquan, July 07 2019
## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 

## The first function in the file, makeCacheMatrix() creates an R object that stores 
## a matrix and its inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        
        ## mtx is set to NULL, initializing it as an object within 
        ## the makeCacheMatrix() environment to be used by later code in the function 
        mtx <- NULL
        
        ## First defines the set() function.
        ## set() takes an argument that is named as y.
        ## It is assumed that this value is a matrix,
        ## but is not stated directly in the function formals.
        
        set <- function(y){
                
                ## Assign the input argument to the x object in the parent environment
                x <<- y
                
                ## Assign the value of NULL to the m object in the parent environment. 
                ## This line of code clears any value of mtx that had been cached by
                ## a prior execution of cacheSolve()
                mtx <<- NULL
        }
        
        
        ## Defines the getter for the matrix x
        get <- function() x
        
        ## Defines the setter for the inverse matrix mtx.
        setinverse <- function(inv) mtx <<- inv
        
        ## Defines the getter for the inverse matrix mtx.
        getinverse <- function() mtx 
        
        ## Create a new object by returning a list(),
        ## assigns each of these functions as an element within a list(), 
        ## and returns it to the parent environment.
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
} 


## The second function, cacheSolve() requires an argument
## that is returned by makeCacheMatrix() in order to retrieve the inverse result from 
## the cached value that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## First, calls the getinverse() function on the input object.
        inv.mtx <- x$getinverse()
        
        ## checks to see whether the result is NULL.
        ## if the value here is not equal to NULL, 
        ## we have a valid, cached inverse result 
        ## and can return it to the parent environment
        if(!is.null(inv.mtx)){
                message("getting cached data")
                return(inv.mtx)
        }
        
        ## If the result of !is.null(inv.mtx) is FALSE, 
        ## cacheSolve() gets the matrix from the input object, 
        ## calculates an inverse result by solve(), 
        ## uses the setinverse() function on the input object 
        ## to set the inverse result in the input object, 
        ## and then returns the value to the parent environment 
        ## by printing the inverse object.
        
        data <- x$get()
        inv.mtx <- solve(data, ...)
        x$setinverse(inv.mtx)
        inv.mtx
}