
##The following 2 functions will cache the inverse of a matrix. This allows the
##inverse of a matrix to be looked up in the cache if it has previously been
##calculated and potentially reduce time-consuming computations.

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		
	##Function to set value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	##Function to get value of the matrix
        get <- function() {
		x
	}

	##Function to set inverse of the matrix
        setInverse <- function(Inverse) {
		        m <<- Inverse
	}

	##Function to get inverse of the matrix
        getInverse <- function() {
		        m
	}
		
##Returns a list containing function to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix. If the inverse has already been calculated (and the matrix 
##has not changed), then the cacheSolve should retrieve the inverse from the 
##cache.

##The input is an object created by makeCacheMatrix
cacheSolve <- function(x, ...) {					 

	##Accesses the object 'x' and gets the value of the matrix inverse
	m <- x$getInverse() 						
		
	##Determine if the inverse has already been calculated.
	##If so, return the inverse from the cache and skip the calculation
	if(!is.null(m)) {					 
	        	message("getting cached data") 
			return(m) 			
	}
	      
	##Access the matrix from object 'x' if the inverse has not already
	##been calculated
	data <- x$get() 					
		      				 
	##If 'm' was NULL (inverse not already calculated), then calculate
	##the inverse of the matrix
	m <- solve(data) %*% data
		
	##Store the calculated value in the cache x via the setInverse function	      
	x$setInverse(m) 
	
       	##returns a matrix that is the inverse of matrix 
        m 									
}