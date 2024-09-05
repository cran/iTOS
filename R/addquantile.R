addquantile <-
function(costmatrix,z,p,pct=c(.2,.4,.6,.8),penalty=1000){
  stopifnot(is.vector(p)&is.vector(z)&(length(z)==length(p)))
  stopifnot(all(!is.na(p)))
  stopifnot(is.vector(pct))
  stopifnot(all(pct>0)&all(pct<1))
  iscore<-as.integer(cut(p,stats::quantile(p,c(0,pct,1)),include.lowest=TRUE))
  addinteger(costmatrix,z,iscore,penalty=penalty)
}
