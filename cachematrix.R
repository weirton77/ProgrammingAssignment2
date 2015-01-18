#in the context of a series of time-consuming calculations which recur in arbitrary sequence,
#we want to save the results of each unique calculation so that it doesn't need to be repeated.
#to implement this, using the analogy of containers, we use one function (makeCacheMatrix)
#to form a container for each calculation, and another (cacheSolve) to perform the calculation 
#and fill the container.  the latter simply reads the results if the calculation has already
#been performed.

#forms a container to hold results of a calculation, to be performed by  cacheSolve
#and puts a label on it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y            
    m <<- NULL
  }
  #labels the container  
  get <- function() x                      
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}              

#check the label on container l, and reject if not the same as x
# otherwise, return a matrix that is the inverse of 'x'

cacheSolve <- function(x, l) {

  if(!identical(l$get(),x)){return(NULL)}  
  m <- l$getmean()
  #if the container is full, calculation has already been performed. return with the result  
  if(!is.null(m)) {                   
    message("getting cached data")
    return(m)
  }
  #otherwise, perform the calculation, put result in the container, and return with the result
  m <- round(solve(x), digits = 4)           #calculation of interest (one time only)
  l$setmean(m)                 
  m
}


#now we need a method to generate a list of the containers, inspect them, and add as needed
#the following function serves that purpose

savemeans <- function(x = matrix()) {
  #check the list of containers to see if any label matches x
  #if you find one, get the result and return
  #otherwise, make a new container, fill it, and add it to the list  
  len <- length(cachelist)
  if (len >= 4){
    for (i in seq(2,len,by=4)){
      if(identical(cachelist[[i]](),x))  {
        mn <- cachelist[[i+2]]()
        return(mn)}
    }
  }
  #if no match ...btw, need to force eval of vec before it will add to the list
  vec<-makeCacheMatrix(x)
  mn <-cacheSolve(x, vec )
  cachelist <<- c(cachelist,vec)  #note the assignment operator, to get it to the global env
  return(mn)
}

#to use this, set cachelist to NULL, and feed input thru savemeans
#this process could easily be converted to a functional, since the only substantive change
#is in the single line of code which performs the calculation
#of course, you would also remove the restriction on x
#FUN would be an arg of savemeans and makecachematrix, but not makeCacheMatrix.  it doesn't care
#and change the names to suggest a more general application