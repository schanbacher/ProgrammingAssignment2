## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function makeing a cache Matrix
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL                                   
        set <- function(y) {                                    # sets the values        
                x <<- y                                           
                invM <<- NULL                                   # set inverted matrix to NULL 
        }    
        get <- function() x                                     # get the matrix
        setinverse <- function(solve) invM <<- solve            # set the inverted matrix   
        getinverse <- function() invM                           # get the inverted matrix   
        list(set = set, get = get,                              # list of available functions       
             setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
# get inverted matrix, if not available calculate it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinverse()                          # save inverted matrix   
        if(!is.null(invM)) {                            # if not NULL    
                message("getting cached data")              # print message "get cached data" and  
                return(invM)                            # return inverted matrix
        }                                               # else if NULL   
        myData <- x$get()                               # get the matrix     
        invM <- solve(myData, ...)                      # calculate inverted matrix  
        x$setinverse(invM)                              # set inverted matrix   
        invM      
}
