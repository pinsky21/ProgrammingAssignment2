
## makeCacheMatrix creates list object from a matrix. The list consists of functions
##      that allow you to set and get both the original matrix passed to the function
##      and the inverse of the matrix IF it has be cached by cachSolve
## cacheSolve takes the list object created by makeCacheMatrix and returns the
##      inverse of the original matrix. If the inverse was previously calculated and
##      stored (i.e. cached) then the stored value is simply returned. If the
##      inverse has not been cached then it is calcuated, stored, and returned.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## inv will be the matrix 'inverse' and it's initialized to NULL
        inv <- NULL
        
        ## creating a setter function (or method) that can be used to reassign
        ## a new matrix to the object created by makeCacheMatrix. When this
        ## is called, the cached inverse is reset to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## creating a getter function (or method) that simply returns the matrix
        get <- function() x
        
        ## creating a setter function to store the inverse of the matrix which 
        ## will be calculated in the cacheSolve function
        setInv <- function(inverse) inv <<- inverse
        
        ## creating a getter function 
        getInv <- function() inv
        
        ## functions usable in the new (list) object created by makeCacheMatrix
        ## names left of the '=' are used with the new (list) object
        ## names right of the '=' are from within this makeCacheMatrix function
        list(set = set, 
             get = get,
             setInverse = setInv,
             getInverse = getInv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve will retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## first retrieve whatever matrix may have been stored as the inverse
        inv <- x$getInverse()
        
        ## if the stored matrix is NOT null then we know we have a cached
        ## inverse. There is no need to calculate it again so simply
        ## return the matrix stored in inv and be done with it!
        if(!is.null(inv)) {
                ## print a message to the console signaling the cached
                ## matrix is being retrieved (i.e. no recalculation)
                message("getting cached data")
                ## returns the cached inverse of matrix 'x'
                return(inv)
        }
        
        ## if we get to this point then we need to calculate the matrix
        ## inverse and store it
        
        ## get the original matrix
        data <- x$get()
        
        ## calculate the inverse of the matrix
        inv <- solve(data)
        
        ## set the inverse (i.e. cache it) so it can be retrieved later
        x$setInverse(inv)
        
        ## Returns a matrix that is the inverse of 'x'
        inv
}
