## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setInverse <-function(inverse) inv<<-inverse
	getInverse <-function(inverse) inv
	list(set=set,get=get,
	     setInverse=setInverse,
	     getInverse=getInverse)
}


## Function calculates the inverse of the special "matrix". It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the  setInverse function.

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        matrix<-x$get()
        inv<-solve(matrix,...)
        x$setInverse(inv)
        print(inv)
}
