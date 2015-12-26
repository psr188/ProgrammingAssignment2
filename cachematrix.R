## This .R file is designed to for speedy calculation
## of inverse of an invertible matrix
## Two functions are used to obtain the inverse matrix
## Caching is used for fast calculations 

##create a function which starts with a null matrix argument
makeCacheMatrix <- function(x = matrix()) { 
        Inv <- NULL ## Inv is the inversematrix. Initialized as "NULL". 
        set <- function(y) {  ## Function to cache the inverse matrix
                x <<- y
                Inv <<- NULL              
        }
        get <- function() x ## Gets the inverse matrix
        setinv <- function(solve) Inv <<- solve ## Calculates the inverse matrix
        getinv <- function() Inv ## Gets the inverse matrix
        list(set = set, get = get,                    
             setinv = setinv,
             getinv = getinv) ## Output of the makeCacheMatrix function
}

## Function used to get the cache of the matrix or solve afresh
## Input x to this function should not be a matrix, but the output of makeCacheMatrix;
## Else x$getinv() will indicate that we cannot apply "$" to an atomic unit - i.e., a matrix

cacheSolve<- function(x, ...) {                  
        Inv <- x$getinv()   
        if(!is.null(Inv)) {                 
                message("calculating inverse by getting cached data")
                return(Inv)
        }
        #if no inverse, it is first calculated and then printed.
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinv(Inv)
        Inv
}
## Example Output

## > M = matrix(c(2,4,10,12),2,2)
## > M1 = makeCacheMatrix(M)

## > cacheSolve(M1)
## [,1]   [,2]
## [1,] -0.75  0.625
## [2,]  0.25 -0.125

## > cacheSolve(M1)
## getting cached data - Inverse of the matrix
## [,1]   [,2]
## [1,] -0.75  0.625
## [2,]  0.25 -0.125
