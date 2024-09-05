addcaliper <-
function(costmatrix,z,p,caliper=NULL,penalty=1000,
                     twostep=TRUE){
  stopifnot(is.logical(twostep)&is.vector(twostep)&(length(twostep)==1))
  stopifnot(is.vector(z))
  stopifnot(is.vector(penalty)&(length(penalty)==1)&(penalty>0))
  stopifnot(all((z == 0) | (z == 1)))
  stopifnot((dim(costmatrix)[1])==sum(z))
  stopifnot((dim(costmatrix)[2])==sum(1-z))
  stopifnot(is.vector(p)&(length(z)==length(p)))
  stopifnot(all(!is.na(p))) # no missing values in p
  if (!is.null(names(z))) nm<-names(z)
  else nm<-1:length(z)
  stopifnot(all(rownames(costmatrix)==nm[z==1]))
  stopifnot(all(colnames(costmatrix)==nm[z==0]))
  if ((!is.null(caliper))&(length(caliper)==2))
    stopifnot((min(caliper)<=0)&(max(caliper)>=0))
  if (is.null(caliper)) caliper<-c(-.2,.2)*stats::sd(p)
  else if (length(caliper)==1) caliper<-c(-1,1)*abs(caliper)
  else if (length(caliper)==2) caliper<-c(min(caliper),max(caliper))
  else {
    warning("Caliper must be (i) NULL or
            (ii) one number, or (iii) three numbers")
    stop()
  }
  dif<-outer(p[z==1],p[z==0],"-")
  o<-((dif>caliper[2])+(dif<caliper[1]))*penalty
  if (twostep) o<-o+((dif>(2*caliper[2]))+(dif<(2*caliper[1])))*penalty
  costmatrix+o
}
