## The two functions collectively create the inverse of a matrix, cache the  
## inverse and retrieve the inverse from the cache if the inverse has already 
## been calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(m=matrix()) {
	p<-NULL
	set<-function(y) {
		m<<-y
		p<<-NULL
	}

	get<-function() m

	setinverse<-function(solve) p<<-solve

	getinverse<-function() p

	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" object created
## by the function above, or retrieves the inverse from the cache if it has
## already been calculated.

cacheSolve<-function(x,...) {
	i<-x$getinverse()
		
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	matrix<-x$get()
	i<-solve(matrix,...)
	x$setinverse(i)
	i

}