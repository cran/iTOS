\name{addquantile}
\alias{addquantile}

\title{
Cut a Covariate at Quantiles and Add a Penalty for Different Quantile Categories
}
\description{
Cut a Covariate at Quantiles and Add a Penalty for Different Quantile Categories
}
\usage{
addquantile(costmatrix, z, p, pct = c(0.2, 0.4, 0.6, 0.8), penalty = 1000)
}

\arguments{
  \item{costmatrix}{
An existing cost matrix with sum(z) rows and sum(1-z) columns.  The function checks the compatability of costmatrix, z and p; so, it may stop with an error if these are not of appropriate dimensions.  In particular, costmatrix may come from startcost().
}
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.
}
  \item{p}{
A vector of length equal to length(z).  Quantiles of p will penalize the distance.
}
  \item{pct}{
A vector of numbers strictly between 0 and 1.  These determine the quantiles of p.  For instance, c(.25,.5,.75) uses the quartiles of p.
}
  \item{penalty}{
One positive number used as a penalty.
}
}
\details{
The vector p is cut at its quantiles defined by pct, and the integer difference in quantile categories is multiplied by penalty and added to the distance matrix.
The function is similar to addinteger(), except the integer values are not
specified, but rather are deduced from the quantiles.

If you cannot match for the quantile category of p, then addquantile() prefers
to match from an adjacent quantile category.
}
\value{
A penalized distance matrix.
}

\author{
Paul R. Rosenbaum
}

\examples{
data(binge)
d<-binge[binge$AlcGroup!="N",]
attach(d)
z<-1*(AlcGroup=="B")
names(z)<-SEQN
dist<-startcost(z)
quantile(age,pct=c(1/4,1/2,3/4))
rbind(z,age)[,1:20]
addquantile(dist,z,d$age,pct=c(1/4,1/2,3/4),penalty=5)[1:5,1:7]
detach(d)
rm(z,dist,d)
}

