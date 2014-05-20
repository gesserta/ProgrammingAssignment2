##These functions take a matrix, check to see if there is a cached version,
##and if not, creates a cached version of the inverse.

##The first function outputs a list of functions that can be called by the second function.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL ##clear the cache
	set<-function(y) {
		x<<-y ##set the input matrix to the cache
		m<<-NULL ##clear the cache
	}
	get <- function() x ##get will display cached matrix
	savecache<-function(solve) m<<-solve ##inverted matrix saved to m
	getcache<-function() m ##getcache will display the cached inverted matrix
	list(set=set, get=get, savecache=savecache, getcache=getcache) ##output list of functions
}


##The second function checks to see if there is a cached inverted matrix.
##If there is a cached result for that matrix, then it outputs the cached inverse.
##If not, then it computes the inverse for the matrix and stores it to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$savecache(m)
        m

}