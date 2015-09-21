## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Computing the matrix inversion can be very computationaly expensive.
# So if you have it in a loop and have to do it many times, it can be really slow.
#Therefore Caching the inverse function may be a good thing to think about doing

# makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
#Set the value of the matrix
    	set <- function(y) { 
	x <<- y 
	inv <<- NULL 
}
# Get the value of the matrix
	get <- function() x 
#Set and get the value of inverse of the matrix

	setinverse <- function(inverse) inv <<- inverse 
	getinverse <- function() inv 
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}

## Write a short comment describing this function
# there is no error checking in this function so it assumes that the matrix is
# always invertible.  
# 1. Check if inverse has already been computed.
# 2. if it has, return that value
# 3. Otherwise, calsulate the inverse and return that value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	# Check to see if the there are cached values
	
	if(!is.null(inv)) { 
		message("getting cached data.") 
		return(inv) 
		} 
	## nothing cached..Calculating inverse
	message("Nothing Cached... calculating inverse")
	data <- x$get() 
	inv <- solve(data) 
	x$setinverse(inv) 
	inv 

}

makeVector <- function(x = numeric()) {
        		m <- NULL
	        set <- function(y) {
		                x <<- y
			        m <<- NULL
			        }
        			get <- function() x
        			setmean <- function(mean) m <<- mean
        			getmean <- function() m
        			list(set = set, get = get,
        			setmean = setmean,
       				getmean = getmean)

  					}

cachemean <- function(x, ...) {
        		m <- x$getmean()
	       		 if(!is.null(m)) {
		             message("getting cached data")
		                return(m)
				        }
		        data <- x$get()
		        m <- mean(data, ...)
		        x$setmean(m)
		        m
			}


#here are some Values you can use to run the program.
#> x = rbind(c(1, 2), c(2, 1))
#> x
#>m = makeCacheMatrix(x)
#> m$get()
#> cacheSolve(m)
#> cacheSolve(m)


#output after the commands were run
#> x = rbind(c(1, 2), c(2, 1))
#> x
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> cacheSolve(m)
#Nothing Cached... calculating inverse
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(m)
#getting cached data.
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333

