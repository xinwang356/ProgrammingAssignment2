## Calculating the inverse of a matrix.
## As the calculation is time-consuming, it is better to 
## compute that one time and retrieve that value instead calculating
## repead times. 

## Taking the advantage of R Lexical scoping property to save the memory.

makeCacheMatrix <- function(x = matrix()) {                 ##  Initialize the object matrix x and its inverse as object inv.
    inv <- NULL
    set <- function(y){                                     ##  Assign the input argument to the x object in the parent environment
            x <<- y                                         ##  Assign the Null value to the inv object in the parent environment
            inv <<- NULL                                    ##  Reseting (clear the memory) for recalculating the inverse after a new
    }                                                       ##  ++ matrix input instead of retrieve the old values. 
        get <- function() x                                 ##  getter to access the object x retrieve data from x
        setinv <- function(inverse) inv <<- inverse         ##  setter used to set data values inverse within the object inv which defined in parent environment 
        getinv <- function() inv                            ##  getter used to retrieve the data value of inversed matrix 
        list (set = set, get = get,                         ##  give the names for the four functions which will point to parent environment when called
              setinv = setinv, getinv = getinv)
}


## Point back to the parent environment. First check if the inverse has null value, if not direct retrieve the data from cache. 
## Otherwise, calculate the inverse for the given data when point back to the function at parent environment and return the calculated values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinv(inv)
        inv
}  
