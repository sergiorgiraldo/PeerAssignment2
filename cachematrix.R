makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
                
        #setup functions
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m

		#extend matrix with new functions
		list(
			set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}

cacheSolve<- function(x, ...) {
        #try to get inverse from cache  
		m <- x$getinverse()
		if(!is.null(m)) {
        #return from cache        
			message("getting cached data")
            return(m)
        }

        #solve inverse and ...
		data <- x$get()
		m <- solve(data)
                
        #store in cache
		x$setinverse(m)

        #return inverse of matrix
		m
}

testCacheSolve <- function() {
		s <- seq(4)
		ms<-matrix(s,2)
		msi<-solve(ms)
		s1<-makeCacheMatrix(ms)
		
		ret1 <- cacheSolve(s1)
		if (!identical(ret1,msi)){
					   stop("error!")
		}
		ret2 <- cacheSolve(s1)

		if (!identical(ret2,ms)){
					   stop("error!")
		}
		message("tests ok")
}
