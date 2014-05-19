## The following functions to create a matrix, cache its inverse and retrieve the same. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
{
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(y) 
{
 ## Return a matrix that is the inverse of 'y'
        inverse <- y$getinv()
        if(!is.null(inverse))
        {
                message("getting cached data")
                return(inverse)
        }
        data <- y$get()
        inverse <- solve(data)
        y$setinv(inverse)
        inverse
}
       
