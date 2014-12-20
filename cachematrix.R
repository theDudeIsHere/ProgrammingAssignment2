#Since inversing a matrix may be costly this functions implement caching so that the operations does not need to be computed repeatedly

#The first function, makeCacheMatrix creates a special "matrix" object that can be cached
#It is really a list containing a functions to
#set the value of the matrix (setmatrix)
#get the value of the matrix (getmatrix)
#set the value of the inverse of the matrix (setinvmatrix)
#get the value of the inverse of the matrix (getinvmatrix)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinvmatrix <- function(inv) inverse <<- inv
    getinvmatrix <- function() inverse
    list(setmatrix=setmatrix, getmatrix=getmatrix, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)

}


#The following function calculates the inverse of the special "matrix" created with the above function. 
#However first it checks to se if the inverse already exists. If that is the case it returns the cached inverse without doing the calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinvmatrix()
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$getmatrix()
    inverse <- solve(data)
    x$setinvmatrix(inverse)
    inverse
}
