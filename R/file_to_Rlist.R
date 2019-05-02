#' Reads output from a single Hydra into an R list
#'
#'Reads in a model run output file of variable length and formats it into a List where each list item is the name of
#'an index calculated in the model. THIS NEED TO BE CODED IN C++ TO SPEED THINGS UP
#'
#' @param fn character. Filename of output to read
#'
#' @return List of variable length
#' \item{indices}{Each list item is an index from the model run where the name of the list item is the name of the index as specified in the outputfile}
#'
#'
#' @export


file_to_Rlist = function(fn) {
  # reads in filename, fn, and interprets evrything as a character
  ifile=scan(fn,what="character",flush=T,blank.lines.skip=F,quiet=T)

  # tries to convert to double."Real" characters wont.
  # We want these since they are variable names
  idx=sapply(as.double(ifile),is.na)
  vnam=ifile[idx] #list the variable names
  nv=length(vnam) #number of names
  A=list()
  ir=0
  # loop through the list of varoable names and get the data associated with each
  for(i in 1:nv)
  {
    ir=match(vnam[i],ifile)
    if(i!=nv){ # not on last element.count lines of data between variable names
      irr=match(vnam[i+1],ifile)
    } else {
      irr=length(ifile)+1
    }
    dum<-NA
    # if a single line, read into a vector
    if(irr-ir==2) dum<-as.double(scan(fn,skip=ir,nlines=1,quiet=T,what=""))
    # otherwise read into a matrix
    if(irr-ir>2) dum<-as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=T))
    #Logical test to ensure dealing with numbers. if so add to list
    if(is.numeric(dum)){
      A[[ vnam[i ]]]<-dum
    }
  }
  # a list of objects, each accessible by its variable name
  return(A)
}
