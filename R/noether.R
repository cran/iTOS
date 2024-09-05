noether <-
function(y,f=2/3,gamma=1,alternative="greater"){
  stopifnot(is.element(alternative,c("greater","less","two.sided")))
  stopifnot(is.vector(gamma)&(length(gamma)==1)&(gamma>=1))
  if (gamma!=1){
    if (alternative=="two.sided")
      stop("alternative = two.sided is available only if gamma=1")
    if (alternative=="less") pr <- 1/(1+gamma)
    else pr <- gamma/(1+gamma)
  }
  else pr<-1/2
  stopifnot(is.vector(y)&(length(y)>=2))
  stopifnot(is.vector(f)&(length(f)==1)&(f>=0)&(f<1))
  ay<-abs(y)
  rk<-rank(ay)
  use<-(ay>0)&((rk/length(y))>=f)
  nn<-sum(use)
  Tn<-sum(y[use]>0)
  pval<-stats::binom.test(Tn,nn,p=pr,alternative=alternative)$p.value
  list(number.pairs=nn,positive.pairs=Tn,pval=pval)
}
