## The function makeCacheMatrix creates an R object that stores an 
## invertible square matrix and its inverse.



## The function cacheSolve computes the inverse of the matrix stored in the
## R object created by the makeCacheMatrix function.
## The cacheSolve function first checks if it can retrieve the inverse of matrix
## x from cache. 

## if the inverse of matrix x was previously calculated, the if statement in the 
## cacheSolve function is interpreted as TRUE. Thus the inverse of x is
## retrieved with the getsolve() function and a message states that the inverse
## was retrieved from cache.

## if the inverse of matrix x has not been calculated before, the matrix is
## retrieved by the get() function, its inverse it calculated by the built-in 
## solve() function, the resulting inverse matrix is stored using the setsolve()
## function. At the end the inverse of x is printed to the console.





## makeCacheMatrix function takes a matrix(assumed to be an invertible square 
## matrix) creates an R object containing the input matrix x, data object s 
## and a list of length 4 containing the functions: set(), get(),
## setsolve() and getsolve().

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list( set = set, get = get, setsolve =setsolve, getsolve = getsolve)
}


## the cacheSolve function takes the R object created by makeCacheMatrix
## function as its argument and returns the inverse of the matrix that was 
## entered as an argument to the makeCacheMatrix function.

## If the inverse of the matrix has been previously computed (by cacheSolve)
## and stored, the cacheSolve function retrieves the inverse
## from the cache(R object created by makeCacheMatrix function)
## and states that the result was retrieved from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}
