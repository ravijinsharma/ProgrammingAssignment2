## The function "makeCacheMatrix" it creates an matrix that can cache its value
## as inverse of the matrix. If the inverse is already calculated than cacheSolve
## retrives the value from cache. Computation of square matrix for inverse and
## its identification as a repetative value are also done by function "cacheSolve".


## MakeCacheMatrix function takes argument as x( matrix). It has set,get,setinverse
## and getinverse functions. functions get and getinverse uses '<<' operator which 
## assigns values out of the function where they are defined.

makeCacheMatrix <- function(x = matrix()) {
	n<-NULL
	set<-function(y){
		x<<-y
		n<<-NULL
	}
	get<-function()x
	setinverse<-function(inverse) n<<-inverse
	getinverse<-function()n
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Function cacheSolve takes argument as x it basically finally compute inverse or 
## retrive the inverse from cache if the input matrix is repeated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	n<-x$getinverse()
	if(!is.null(n)){
		message("getting Cached Data")
		return(n)
	}
	data<-x$get()
	n<-solve(data,...)
	x$setinverse(n)
	n
}
