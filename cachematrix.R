## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }        #Define the set function which initiate x and iv.
    get <- function() x   #Define the get function which returns value x.
    setinverse <- function(inverse) iv <<- inverse  #Define the set function which value variable iv as the parameter.
    getinverse <- function() iv #Define the get function which returns value iv.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    iv <- x$getinverse() #First try to use the iv value in x. 
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    } #If there is cached iv, show a message and return the cached iv directly. 
    data <- x$get() #If there is no cached iv, get the matrix in x.
    iv <- solve(data, ...)  # Calculate inverse of the matrix.
    x$setinverse(iv) # Value iv in x.
    iv #Return a matrix that is the inverse of 'x'
}
