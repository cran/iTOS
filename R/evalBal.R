evalBal <-
function(z,x,statistic="s",reps=1000,
                  trunc=.2,nunique=2,alpha=0.05){
  stopifnot(all((z==1)|(z==0)))
  stopifnot((length(z)/sum(z))==round(length(z)/sum(z)))
  stopifnot(is.matrix(x)|is.data.frame(x)|is.vector(x))
  if (is.vector(x)) {
    stopifnot(length(x)==length(z))
    x<-matrix(x,length(x),1)
  }
  else stopifnot(length(z)==(dim(x)[1]))
  if (statistic=="t"){
    tstat<-function(v1,v0){t.test(v1,v0)$p.value}
    test.name="two-sample t-statistic"
  }
  else if (statistic=="w"){
    tstat<-function(v1,v0){wilcox.test(v1,v0)$p.value}
    test.name="two-sample Wilcoxon-statistic"
  }
  else if (statistic=="s") {
    test.name="two-sample Wilcoxon-statistic or chi-square"
    tstat<-function(v1,v0){
    nu<-length(unique(c(v1,v0)))
    if ((nu<=nunique)|(is.factor(c(v1,v0)))) {
      w<-c(rep(1,length(v1)),rep(0,length(v0)))
      tb<-table(c(v1,v0),w)
      chisq.test(tb)$p.value
    }
    else wilcox.test(v1,v0)$p.value
    }
  }

  # Initialize
  J<-dim(x)[2]
  actual<-rep(NA,J+3)
  sim<-matrix(NA,reps,J+3)
  if (!is.null(colnames(x))) {
    names(actual)<-c(colnames(x),"minP","tProduct","nLEalpha")
    colnames(sim)<-c(colnames(x),"minP","tProduct","nLEalpha")
  }

  # Evaluate actual match
  for (j in 1:J) actual[j]<-tstat(x[z==1,j],x[z==0,j])
  actual[J+1]<-min(actual[1:J])
  if (all(actual[1:J]>trunc)) actual[J+2]<-1
  else actual[J+2]<-prod((actual[1:J])[actual[1:J]<=trunc])
  actual[J+3]<-sum(actual[1:J]<=alpha)

  # Simulated randomized experiments
  for (i in 1:reps){
    zi<-sample(z,length(z),replace=FALSE)
    for (j in 1:J){
      sim[i,j]<-tstat(x[zi==1,j],x[zi==0,j])
    }
    sim[i,J+1]<-min(sim[i,1:J])
    if (all(sim[i,1:J]>trunc)) sim[i,J+2]<-1
    else sim[i,J+2]<-prod((sim[i,1:J])[sim[i,1:J]<=trunc])
    sim[i,J+3]<-sum(sim[i,1:J]<=alpha)
  }
  sim<-as.data.frame(sim)
  o<-apply(outer(rep(1,reps),actual,"*")<sim,2,sum)
  o[J+3]<-sum(actual[J+3]>sim[,J+3]) # reverse direction for nLEalpha
  names(o)<-colnames(sim)
  list(test.name=test.name,actual=actual,simBetter=o,sim=sim)
}
