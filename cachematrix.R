## The overall functions working together will determine if the inverse of the matrix
##needs to be calculated or if it can be puled from cache due to prior calculation
##this saves on computing time.

## the Functions:
## 1.set the matrix 
##2.get the matrix 
##3.calculate and set the inverse 
##4.get the inverse.

## The makeCacheMatrix will actually store the inverse of the matrix so it 
##can be comparedagainst when cachesolve is run again. This is passed to it  
##by cachesolve where it is stored in m if m is null it dot retreive from cache


makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## this function will return the inverse of the matrix, it will determine if it
##has already been stored if the matrix has not changed, if it has not changed it
##will retreive the already calculated inverse for this matrix otherwise it will
##calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("We are pleased to provide you cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        return(m)
}
