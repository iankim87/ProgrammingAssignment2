## The following functions allow 1) the creation of a special matrix object 
# that enables the caching of its inverse and 2) the retrieval of the cached
# inverse (or computation of the inverse if it is not stored)


## This "makeCacheMatrix" function takes as input a numeric matrix and creates a special matrix
# object - a list of functions to:
# 1. set the matrix
# 2. get the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # set matrix
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        
        # get matrix
        get <- function() x 
        
        # set inverse
        setinverse <- function(inverse) inv <<- inverse 
        
        # get inverse
        getinverse <- function() inv 
        
        # return special matrix object
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This "cacheSolve" function takes as input the special matrix object created above,
# and returns its inverse, by either retrieving the cached value or 
# calculating the inverse (and caching it)

cacheSolve <- function(x) {
        
        # attempt to get inverse from the special matrix object
        inv <- x$getinverse()
        
        # if inverse exists (is cached), return it
        if(!is.null(inv)) {
                message("Retrieving cached data")
                return(inv)
        }
        
        
        # if inverse does not exist (it is NULL), calculate the inverse
        # first get the matrix
        data <- x$get() 
        
        # solve for the inverse, and cache it
        inv <- solve(data) 
        x$setinverse(inv) 
        
        # return the inverse matrix
        inv 
}

