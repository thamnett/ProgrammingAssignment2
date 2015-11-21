## Matrix inversion is usually a costly computation
## Thus, the below will cache the inverse of a matrix 
## rather than compute it repeatedly
## The following pair of functions cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Check for square matrix and exit if not
        d <- dim(x)
        if(d[1] != d[2]) {
                message("Plesae enter square matrix")
                return
        }
  
        ## assign m variable to NULL
        m <- NULL
        
        ## set is a function that assigns the y argument
        ## to the original matrix and sets m to NULL, both
        ## in the global environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get is a function that returns the orignal matrix
        get <- function() x
        
        ## setinv assigns m to the passed inverse matrix
        ## in the global environment
        setinv <- function(solve) m <<- solve
        
        ## getinv returns m
        getinv<- function() m
        
        ## return a a special "matrix" object that can cache its inverse
        list(set = set, get = get,
                setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" 
## returned by makeCacheMatrix aboved

cacheSolve <- function(x, ...) {
        ## Assigns m to the inverse matrix if it has already been calculated
        m <- x$getinv()
        
        ## Checks to see if the inverse has already been calculated
        ## If so, it gets the inverse from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If inverse has not been calculated
        
        ## Assigns the orignal matrix to data
        data <- x$get()
        
        ## Assigns m to inverse of matrix 
        m <- solve(data, ...)
        
        ## Sets the value of the inverse in the cache via setinv function
        x$setinv(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
