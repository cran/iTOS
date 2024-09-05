makematch <-
function(dat,costL,costR,ncontrols=1,controlcosts=NULL,solver="rlemon"){

  stopifnot((solver=="rlemon")|(solver=="rrelaxiv"))
  stopifnot(is.matrix(costR))
  stopifnot(is.matrix(costL))
  stopifnot((dim(costR)[1])==(dim(costL)[1]))
  stopifnot((dim(costR)[2])==(dim(costL)[2]))
  stopifnot(is.vector(ncontrols)&(length(ncontrols==1))&(ncontrols>=1))
  stopifnot(ncontrols==round(ncontrols))
  if (!is.null(controlcosts)) {
    stopifnot((dim(costR)[2])==length(controlcosts))
    stopifnot(all(controlcosts>=0))
  }
  else controlcosts<-rep(0,dim(costR)[2])
  stopifnot(all(rownames(costR)==rownames(costL)))
  stopifnot(all(colnames(costR)==colnames(costL)))
  stopifnot(is.data.frame(dat)|is.matrix(dat))
  stopifnot((dim(dat)[1])==sum(dim(costR)))
  stopifnot(all(is.element(rownames(dat),c(rownames(costR),colnames(costR)))))

  net<-makenetwork(costL,costR,ncontrols=ncontrols,
                   controlcosts=controlcosts)$net

  result<-rcbalance::callrelax(net,solver=solver)
  if (result$crash==1) {
    warning("callrelax crashed")
    stop()
  }
  if (result$feasible==0) {
    warning("problem is infeasible")
    stop()
  }


  pairs<-result$x[1:(dim(costR)[1]*dim(costR)[2])]
  pairs<-matrix(pairs,dim(costR)[1],dim(costR)[2])
  rownames(pairs)<-rownames(costR)
  colnames(pairs)<-colnames(costR)

  o<-NULL
  for (i in 1:(dim(costR)[1])){
    o<-rbind(o,dat[which(rownames(dat)==(rownames(pairs)[i])),])
    w<-colnames(pairs)[pairs[i,]==1]
    if (length(w)!=ncontrols) browser()
    stopifnot(length(w)==ncontrols)
    o<-rbind(o,dat[is.element(rownames(dat),w),])
  }
  mset<-gl(dim(pairs)[1],ncontrols+1)
  o<-cbind(o,mset)
  o
}
