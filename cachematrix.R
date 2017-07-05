## The functions have comments line-by-line.  The first creates an environment in which there is a matrix and ##inverse.  It also creates functions inside the environment that can report the matrix and the inverse and change the matrix, rendering the inverse NULL.

##The second function accesses the environment of the first function and checks to see if the inverse matrix is still reported, or if it's NULL.  If it is NULL, it accesses the matrix and calculates the inverse.  Then stores the inverse in the first environment.

makeCacheMatrix <- function(size,x) {
	inverse <- NULL ##Empties out the inverse variable.
	
	m<-matrix(rnorm(size^2),size,size) ##Creates a square matrix, invertible matrix of dimension specified by "size" argument.
	
	if (x==1){ inverse<-solve(m) } ##If and only if argument x = 1, inverts m and stores in "inverse".
	
	set <- function(s) ##Creates a function in the parent environment that changes the matrix, m. 
		{m<<-matrix(rnorm(s^2),s,s)  ##Creates new matrix
			inverse<<-NULL ##Since the matrix has changed, previously solved inverses must be erased.
			}
	
	get <- function() m ##Creates a function that gets the value of m from the parent environment, the current matrix.
	
	getinverse <- function() inverse ##Creates a function that gets of the value of "inverse" from parent environment, the cached inverse.
	
	setinverse <- function(r) {inverse<<-r} ##Creates a function that takes an input from the cacheSolve function and stores it in the inverse variable of the makeCacheMatrix environment.
	
list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)

}


##The second function accesses the environment of the first function and checks to see if the inverse matrix is still reported, or if it's NULL.  If it is NULL, it accesses the matrix and calculates the inverse.  Then stores the inverse in the first environment.

cacheSolve <- function(x, ...) {
  
	cacheinverse <- x$getinverse() ##Gets the inverse created by makeCacheMatrix, if it exists, or else NULL value
	
	if(!is.null(cacheinverse))	{	##If the value now in cacheinverse is not NULL, it reports the value, with a message, letting the user know it came from cache
		message("Cached Inverse:")
		return(cacheinverse)
	}
	
	else { 
		m<-x$get() ##If the value in the cacheinverse is NULL, then first retrieve matrix from the makeCacheMatrix environment 
		
		calcinverse<-solve(m) ##Then calculate the inverse of the retrieved matrix.
		
		x$setinverse(calcinverse) ##Stores the calculated inverse in the makeCacheMatrix environment.
		
		message("Calculated Inverse:") ##Let's user know that the inverse was just now calculated.
		calcinverse ##prints out the inverse that was just now calculated.
		
		}
	

}
