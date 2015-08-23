## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
#         
        get<-function()x
        
        setsolve<-function(solve) m<<-solve
        
        getsolve<-function()m
        
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve
             )

}


## Write a short comment describing this function
##this function calculates the inverse of the matrix or gets from the cached data if it's exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        
        if(!is.null(m)){
                message("getting cached inverse of the matrix")
                return(m)
                
        }
        
        data <- x$get()
        m <- solve(data,LINPACK = FALSE,...)
        x$setsolve(m)
        m
		
}
