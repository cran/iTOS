addinteger <-
function(costmatrix,z,iscore,penalty=1000){
  stopifnot(is.vector(z))
  stopifnot(is.vector(penalty)&(length(penalty)==1)&(penalty>0))
  stopifnot(all((z == 0) | (z == 1)))
  stopifnot((dim(costmatrix)[1])==sum(z))
  stopifnot((dim(costmatrix)[2])==sum(1-z))
  stopifnot(is.vector(iscore)&(length(z)==length(iscore)))
  stopifnot(all(!is.na(iscore))) # no missing values in iscore
  if (!is.null(names(z))) nm<-names(z)
  else nm<-1:length(z)
  stopifnot(all(rownames(costmatrix)==nm[z==1]))
  stopifnot(all(colnames(costmatrix)==nm[z==0]))
  o<-abs(outer(iscore[z==1],iscore[z==0],"-"))*penalty
  costmatrix+o
}
