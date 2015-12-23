## Three functions makeCacheMatrix, cacheSolve, testCache are defined below
## The functions makeCacheMatrix and cacheSolve are used to solve matrix 
## inverse. They are solved only if a solved matrix for the data is not
## already available in the cache. The function testCache can be used to test
## if the solved matrix is being cached.

## The function makeCacheMatrix is used as a cache for a solved matrix. 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solved) i <<- solved
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## The function cacheSolve returns a matrix that is the inverse of 'x'.  
## It checks if the inverse is already available in cache. 
## If yes, it returns the inverse from cache. If no, it computes the inverse 
# and passes it to the cache. Matrix is checked if it is invertible.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        # Check if matrix can be computed for invertibility
        if (!det(abs(data))){
                message("Error: Matrix is singular and cannot be inverted")
                return(0)
        }
        i <- solve(data, ...)
        x$setinv(i)
        i
}

## Use below function to test if cache works. In command line create a square
## matrix and pass this matrix to testCache. 
## e.g. mat1<-matrix(c(2,4,6,8,10,20,14,16,18), nrow = 3, ncol = 3)
## mat2<-matrix(c(2,4,6,8,20,12,14,16,18), nrow = 3, ncol = 3)
## res<-testCache(mat1, mat2)
## Print res to see matrix inverse of mat1. You will also see the number of 
## times matrix inverse was returned from cache. Note that matrix has to be
## squareand non-singular (i.e. det(mat)!=0)
testCache <- function(x, y){
        C<-makeCacheMatrix(x)
        D<-makeCacheMatrix(y)
        # Will solve C and D once. Thereafter it will always access the cache
        for(i in 1:5){
                I<-cacheSolve(C)
                I<-cacheSolve(D)
                I<-cacheSolve(C)
        }
        return(I)
}
