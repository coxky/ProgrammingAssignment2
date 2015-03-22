## The two functions below cache a potentially time-consuming computation 
## involving the inverse of a matrix and allow it to be called or computed
##when necessary.  

## The first function creates a matrix object that can cache its inverse.
## The function requires a matrix as an argument.  The remainder of the
## function allows it to test if a matrix is the inverse (no computation 
##needed so use cached matrix) or the function uses the solve function
## to compute the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function finds the inverse of a matrix but uses the previous
## function to check if comutation of a new matrix is necessary or
## if the inverse of the matrix is already stored.  Note the use of
## <<- in the previous function which stores the variables m and x
## in the broader environment.  This allows the second function to 
## utilize these variables.

        
cacheSolve <- function(x, ...) {        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}






