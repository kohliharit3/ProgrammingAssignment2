## The 2 functions here ensure that if the inverse of a specific matrix
## has already been calculated, we do not have to calculate it again.


## The foll. function creates a list which can store a matrix, and it's mean
## as it contains the functions that do so.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        

}


## This function takes the input from the prev. function, and if the matrix
## is one whose inverse has already been calculated and stored, we don't 
## recalculate it. Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        if(!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## m (the inverse of 'x') is returned
}
