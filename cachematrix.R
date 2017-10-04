## The following two functions together calculates the inversion of a matrix. 
##The first function makeCacheMatrix initiates and stores the values calculated in the second cacheSolve function.
## 

# makeCacheMatrix creates a list with four functions (get,set,getinverse,setinverse) and the two elements m and x.
#makeCacheMatrix takes x as an argument and requires it to be a matrix.
#Initially it sets inverse to NULL which allows us to check if we have a NULL value or a cached value in the next function.
#It then creates "setters" and "getters" to be used later and stores it all in a list as named objects.


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
}
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function takes the previous function makeCacheMatrix as an input. It retrieves the inverse data anc checks if the inverse matrix has been computes or if it is NULL. 
#If so it computes the inverted matrix and saves it in list of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached invertec matrix data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
