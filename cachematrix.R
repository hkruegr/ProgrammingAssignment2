## makeCacheMatrix() 
## Input: invertable matrix 
## Output: list of functions which 
##          1.set the value of the matrix,
##          2.get the value of the matrix,
##          3.set the value of the inverse,
##          4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # initialise Value to store inverse of matrix
    i <- NULL
    #Anonymous function to set value of matrix
    set <- function(y) {
        x <<- y
        # Matrix has changed -> reset "i"
        i <<- NULL
    }
    # Get value of argument x
    get <- function() x;  
    # Store inverted matrix
    setinverse <- function(inverse) i <<- inverse
    # Get inverted matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

    
}


## cacheSolve()
## Input: special matrix as created by makeCacheMatrix, i.e. list of functions
## Output: inverted matrix

cacheSolve <- function(x, ...) {
    
    ## Get stored inverse from special matrix
    i <- x$getinverse()
    # If stored inverse is not NULL, return stored value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # else get value of stored matrix
    data <- x$get()
    # compute inverse for matrix
    i <- solve(data, ...)
    # store inverse into special matrix 
    x$setinvers(i)
    # return inverse
    i
}
